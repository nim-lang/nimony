#
#
#            Nimony's Standard Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Plugin backing `std/regex`'s `lex` construct — the lexer generator.
##
## Input (`loadPluginInput`), for the two spellings of `lex`:
##
##   (stmts lex <s> <pos> (of (ranges <pat>…) <body>)… [(else <body>)])
##   (stmts lex <s>       (of (ranges <pat>…) <body>)… [(else <body>)])
##
## All the patterns go into **one** DFA (`regexcore.rulesToDfa`), and that DFA
## is emitted as a `case` over its states. So a `lex` with a hundred keywords
## costs per character what a `lex` with one costs — which is the whole reason
## a lexer generator exists.
##
## The emitted shape (nifler's "nim-parsed" dialect, re-sem'd in place):
##
##   (stmts
##     (var :st … <startState>) (var :lastRule … 0) (var :lastPos … <pos>)
##     (while true
##       (stmts
##         (var :ch … '\0')
##         (if (elif (infix < <pos> (call len <s>))
##                   (stmts (asgn ch (at <s> <pos>)))))
##         (case st
##           (of (ranges <n>)
##             (stmts
##               [ (asgn lastRule <rule>) (asgn lastPos <pos>) ]   # accepting
##               (if (elif <charTest> (stmts (call inc <pos>) (asgn st <dest>)))…
##                   (else (stmts (break .))))))…
##           (else (stmts (break .))))))
##     (asgn <pos> lastPos)
##     (case lastRule
##       (of (ranges <rules of branch j>…) <body j>)…
##       (else <else body, or (discard .)>)))
##
## `lastRule`/`lastPos` are what make this a real maximal-munch lexer rather
## than lexim's: the automaton runs on past an accepting state looking for a
## longer match, and when it finally gets stuck it *rewinds* to the last
## accepting position. Without that, input that walks a few characters into a
## pattern and then fails would leave `pos` moved and run no branch at all.

import plugins
import std / private / regexcore

# ---------------------------------------------------------------------------
# Reading the input
# ---------------------------------------------------------------------------

type
  Branch = object
    patterns: seq[string] ## one `of` may list several
    body: NifCursor
    isElse: bool

  Input = object
    s: NifCursor          ## the string being matched
    pos: NifCursor        ## the scan position; only valid when `scanning`
    scanning: bool        ## `lex s, pos:` rather than `lex s:`
    branches: seq[Branch]
    err: string
    errAt: NifCursor

proc isBranchTag(n: NifCursor): bool {.inline.} =
  n.kind == TagLit and n.otherKind in {OfU, ElseU}

proc patternText(n: NifCursor; ok: var bool): string =
  ## The text of a pattern literal. A raw string (`r"\d+"`, which is what these
  ## almost always are) reaches the plugin as `(suf "\d+" "R")`, so the literal
  ## has to be unwrapped rather than read straight off the token.
  ok = false
  result = ""
  var c = n
  if c.kind == StrLit:
    ok = true
    result = stringValue(c)
  elif c.kind == TagLit and c.tagText == "suf":
    var f = firstChild(c)
    if f.kind == StrLit:
      ok = true
      result = stringValue(f)

proc readInput(n: NifCursor; name: string; scanning: bool): Input =
  ## `scanning` comes from which template was called, not from the shape of the
  ## arguments: `rematch s, x:` should be rejected as a mistake rather than
  ## quietly turn into a scanner.
  result = Input(s: n, pos: n, scanning: scanning, branches: @[], err: "",
                 errAt: n)
  var args = callArgs(n)
  if not args.hasMore:
    result.err = "`" & name & "` needs the string to match"
    return
  result.s = args
  skip args
  if scanning:
    if not args.hasMore or isBranchTag(args):
      result.err = "`lex` needs the scan position: `lex s, pos:`"
      return
    result.pos = args
    skip args
  while args.hasMore:
    if not isBranchTag(args):
      result.err = "`" & name & "` expects `of <pattern>:` branches"
      result.errAt = args
      return
    if args.otherKind == ElseU:
      var e = firstChild(args)
      result.branches.add Branch(patterns: @[], body: e, isElse: true)
      skip args
    else:
      var b = firstChild(args) # (ranges …)
      var pats: seq[string] = @[]
      if b.kind == TagLit and b.otherKind == RangesU:
        var p = firstChild(b)
        while p.hasMore:
          var ok = false
          let text = patternText(p, ok)
          if not ok:
            result.err = "a `" & name & "` pattern must be a string literal"
            result.errAt = p
            return
          pats.add text
          skip p
      skip b # past (ranges …); `b` now sits on the body
      result.branches.add Branch(patterns: pats, body: b, isElse: false)
      skip args
  if result.branches.len == 0:
    result.err = "`" & name & "` needs at least one `of <pattern>:` branch"

# ---------------------------------------------------------------------------
# Emitting
# ---------------------------------------------------------------------------

proc addCharSetLit(dest: var NifBuilder; cc: set[char]; info: LineInfo) =
  ## `{'a'..'z', '_'}`. Runs are folded so the emitted set is readable in a
  ## `--verbose` dump and cheap to sem, rather than 26 separate elements.
  dest.withTree CurlyX, info:
    var c1 = '\0'
    while true:
      if c1 in cc:
        var c2 = c1
        while c2 < '\xFF' and succ(c2) in cc: c2 = succ(c2)
        if c1 == c2:
          dest.addCharLit c1
        elif c2 == succ(c1):
          dest.addCharLit c1
          dest.addCharLit c2
        else:
          dest.withTree InfixX, info:
            dest.addIdent ".."
            dest.addCharLit c1
            dest.addCharLit c2
        c1 = c2
      if c1 >= '\xFF': break
      inc c1

proc addInSet(dest: var NifBuilder; ch: SymId; cc: set[char];
              info: LineInfo) =
  dest.withTree InfixX, info:
    dest.addIdent "in"
    dest.addSymUse ch, info
    dest.addCharSetLit cc, info

proc addEqChar(dest: var NifBuilder; ch: SymId; c: char; info: LineInfo) =
  dest.withTree InfixX, info:
    dest.addIdent "=="
    dest.addSymUse ch, info
    dest.addCharLit c

proc addIntVar(dest: var NifBuilder; name: SymId; value: int;
               info: LineInfo) =
  dest.withTree VarS, info:
    dest.addSymDef name, info
    dest.addEmptyNode3 info
    dest.addIntLit value

proc addAsgnInt(dest: var NifBuilder; name: SymId; value: int;
                info: LineInfo) =
  dest.withTree AsgnS, info:
    dest.addSymUse name, info
    dest.addIntLit value

proc addBreak(dest: var NifBuilder; info: LineInfo) =
  dest.withTree BreakS, info:
    dest.addEmptyNode info

proc addAdvance(dest: var NifBuilder; pos: NifCursor; info: LineInfo) =
  ## `inc <pos>`. `bindSym`, not an ident: this lands in the caller's scope and
  ## must not pick up whatever they happen to have called `inc`. The operators
  ## below (`<`, `in`, `==`, `..`) stay idents on purpose — binding them would
  ## emit a symbol choice over every overload in scope at each of the hundreds
  ## of places the automaton compares a character, and an operator shadowed by
  ## something non-callable fails loudly rather than silently.
  dest.withTree CallS, info:
    dest.bindSym "inc"
    dest.addSubtree pos

proc addStateBranch(dest: var NifBuilder; dfa: Dfa; src: int;
                    st, ch, lastRule, lastPos: SymId; pos: NifCursor;
                    info: LineInfo; err: var string) =
  ## One `of <src>:` of the state machine.
  let rule = getRule(dfa, state(dfa, src))
  dest.withTree OfU, info:
    dest.withTree RangesU, info:
      dest.addIntLit src
    dest.withTree StmtsS, info:
      if rule != 0:
        # An accepting state: remember how far we got and with which rule, then
        # keep going — a longer match may still be ahead.
        dest.addAsgnInt lastRule, rule, info
        dest.withTree AsgnS, info:
          dest.addSymUse lastPos, info
          dest.addSubtree pos

      # Collect the outgoing edges first so we know whether an `(if …)` is
      # needed at all: `(if)` with no branch is not a tree sem accepts.
      var dests: seq[int] = @[]
      var sets: seq[set[char]] = @[]
      var singles: seq[char] = @[]
      var singleDests: seq[int] = @[]
      for d in allDests(dfa, state(dfa, src)):
        let (others, cs) = allTransitions(dfa, state(dfa, src), d)
        for o in others:
          if o.kind == reChar:
            singles.add o.val
            singleDests.add int(d)
          else:
            err = "a generated matcher cannot use anchors, word " &
                  "boundaries, captures or back references"
        if cs != {}:
          sets.add cs
          dests.add int(d)

      if sets.len == 0 and singles.len == 0:
        dest.addBreak info
      else:
        dest.withTree IfS, info:
          for i in 0 ..< sets.len:
            dest.withTree ElifU, info:
              dest.addInSet ch, sets[i], info
              dest.withTree StmtsS, info:
                dest.addAdvance pos, info
                dest.addAsgnInt st, dests[i], info
          for i in 0 ..< singles.len:
            dest.withTree ElifU, info:
              dest.addEqChar ch, singles[i], info
              dest.withTree StmtsS, info:
                dest.addAdvance pos, info
                dest.addAsgnInt st, singleDests[i], info
          dest.withTree ElseU, info:
            dest.withTree StmtsS, info:
              dest.addBreak info

proc addMachine(dest: var NifBuilder; dfa: Dfa; s, pos: NifCursor;
                st, ch, lastRule, lastPos: SymId; info: LineInfo;
                err: var string) =
  ## The scanning loop itself: everything from `var st` to the end of the
  ## `while`.
  dest.addIntVar st, dfa.startState, info
  dest.addIntVar lastRule, 0, info
  dest.withTree VarS, info:
    dest.addSymDef lastPos, info
    dest.addEmptyNode3 info
    dest.addSubtree pos

  dest.withTree WhileS, info:
    # not `bindSym`: `true` is not a symbol in Nimony, it is the `(true)` node,
    # and the ident is what sem turns back into one
    dest.addIdent "true"
    dest.withTree StmtsS, info:
      # var ch = '\0'
      dest.withTree VarS, info:
        dest.addSymDef ch, info
        dest.addEmptyNode3 info
        dest.addCharLit '\0'
      # if <pos> < len(<s>): ch = <s>[<pos>]
      #
      # `'\0'` past the end is not a sentinel hack: no pattern can match it,
      # because every character class the parser builds starts at `'\1'`.
      dest.withTree IfS, info:
        dest.withTree ElifU, info:
          dest.withTree InfixX, info:
            dest.addIdent "<"
            dest.addSubtree pos
            dest.withTree CallX, info:
              dest.bindSym "len"
              dest.addSubtree s
          dest.withTree StmtsS, info:
            dest.withTree AsgnS, info:
              dest.addSymUse ch, info
              dest.withTree AtX, info:
                dest.addSubtree s
                dest.addSubtree pos
      dest.withTree CaseS, info:
        dest.addSymUse st, info
        for src in 1 .. dfa.stateCount:
          dest.addStateBranch dfa, src, st, ch, lastRule, lastPos, pos, info, err
        dest.withTree ElseU, info:
          dest.withTree StmtsS, info:
            dest.addBreak info

proc addDispatch(dest: var NifBuilder; inp: Input; ruleToBranch: seq[int];
                 lastRule: SymId; info: LineInfo) =
  ## `case lastRule` over the user's bodies. Rules that share a branch (`of
  ## "a", "b":`) are listed in one `of`, so a body is emitted exactly once.
  dest.withTree CaseS, info:
    dest.addSymUse lastRule, info
    for j in 0 ..< inp.branches.len:
      if inp.branches[j].isElse: continue
      var rules: seq[int] = @[]
      for r in 0 ..< ruleToBranch.len:
        if ruleToBranch[r] == j: rules.add r + 1
      if rules.len == 0: continue
      dest.withTree OfU, info:
        dest.withTree RangesU, info:
          for r in rules: dest.addIntLit r
        dest.addSubtree inp.branches[j].body
    dest.withTree ElseU, info:
      var wroteElse = false
      for j in 0 ..< inp.branches.len:
        if inp.branches[j].isElse and not wroteElse:
          dest.addSubtree inp.branches[j].body
          wroteElse = true
      if not wroteElse:
        dest.withTree StmtsS, info:
          dest.withTree DiscardS, info:
            dest.addEmptyNode info

# ---------------------------------------------------------------------------
# `re`: a finished automaton as a literal
# ---------------------------------------------------------------------------

proc addOpcode(dest: var NifBuilder; o: RegexOpcode) =
  ## Spelled out rather than emitted as an ordinal: `bindSym` resolves in this
  ## module's scope, so the reference survives whatever the call site happens to
  ## have named `opcRet`.
  case o
  of opcRet: dest.bindSym "opcRet"
  of opcTestSet: dest.bindSym "opcTestSet"
  of opcTestChar: dest.bindSym "opcTestChar"
  of opcTJmp: dest.bindSym "opcTJmp"
  of opcBegin: dest.bindSym "opcBegin"
  of opcEnd: dest.bindSym "opcEnd"
  of opcWordBound: dest.bindSym "opcWordBound"
  of opcCaptureBegin: dest.bindSym "opcCaptureBegin"
  of opcCaptureEnd: dest.bindSym "opcCaptureEnd"
  of opcBackref: dest.bindSym "opcBackref"

template withSeqLit(dest: var NifBuilder; info: LineInfo; body: untyped) =
  ## `@[ … ]`, which the parser spells `(prefix @ (bracket …))`.
  dest.withTree PrefixX, info:
    dest.bindSym "@"
    dest.withTree BracketX, info:
      body

proc addInstrLit(dest: var NifBuilder; i: RegexInstr; info: LineInfo) =
  dest.withTree OconstrX, info:
    dest.bindSym "RegexInstr"
    dest.withTree KvX, info:
      dest.addIdent "opc"
      dest.addOpcode i.opc
    dest.withTree KvX, info:
      dest.addIdent "arg"
      dest.addIntLit int(i.arg)

proc addRegexLit(dest: var NifBuilder; r: Regex; info: LineInfo) =
  ## The whole compiled automaton as an object constructor. Nothing is built at
  ## run time: the subset construction ran while the program was compiled.
  dest.withTree OconstrX, info:
    dest.bindSym "Regex"
    dest.withTree KvX, info:
      dest.addIdent "code"
      dest.withSeqLit info:
        for i in 0 ..< r.code.len:
          dest.addInstrLit r.code[i], info
    dest.withTree KvX, info:
      dest.addIdent "data"
      dest.withSeqLit info:
        for i in 0 ..< r.data.len:
          dest.addCharSetLit r.data[i], info
    dest.withTree KvX, info:
      dest.addIdent "startAt"
      dest.addIntLit r.startAt
    dest.withTree KvX, info:
      dest.addIdent "captures"
      dest.addIntLit r.captures
    dest.withTree KvX, info:
      dest.addIdent "capCode"
      dest.withSeqLit info:
        for i in 0 ..< r.capCode.len:
          dest.addInstrLit r.capCode[i], info
    dest.withTree KvX, info:
      dest.addIdent "capData"
      dest.withSeqLit info:
        for i in 0 ..< r.capData.len:
          dest.addCharSetLit r.capData[i], info
    dest.withTree KvX, info:
      dest.addIdent "capStartAt"
      dest.addIntLit r.capStartAt

proc flagsOf(n: NifCursor; flags: var set[RegexFlag]; err: var string) =
  ## Reads a `{reExtended, …}` set constructor. The argument arrives
  ## sem-checked, so its members are resolved symbols and only the stem before
  ## the first `.` is the name.
  var c = n
  if c.kind != TagLit:
    err = "the flags must be a set literal such as `{reNoCaptures}`"
    return
  var m = firstChild(c)
  while m.hasMore:
    if m.kind == Symbol or m.kind == Ident:
      var name = ""
      let full = if m.kind == Symbol: symText(m) else: identText(m)
      for ch in full:
        if ch == '.': break
        name.add ch
      case name
      of "reExtended": flags.incl reExtended
      of "reNoBackrefs": flags.incl reNoBackrefs
      of "reNoCaptures": flags.incl reNoCaptures
      else: discard # the type operand of the set constructor
    skip m

proc transformRe(n: NifCursor): NifBuilder =
  let info = n.info
  var args = callArgs(n)
  if not args.hasMore:
    return errorTree("`re` needs a pattern", n)
  var ok = false
  let pattern = patternText(args, ok)
  if not ok:
    return errorTree("`re` needs a string literal; use `tryRe` for a pattern " &
                     "that is only known at run time", args)
  skip args
  var flags: set[RegexFlag] = {}
  var err = ""
  if args.hasMore:
    flagsOf(args, flags, err)
    if err.len > 0: return errorTree(err, args)
  else:
    flags = {reExtended}

  var compiled = emptyRegex()
  if not compileRegex(pattern, flags, compiled, err):
    return errorTree(err, n)
  result = createTree()
  result.addRegexLit compiled, info

proc transform(n: NifCursor; name: string): NifBuilder =
  let info = n.info
  var inp = readInput(n, name, name == "lex")
  if inp.err.len > 0:
    return errorTree(inp.err, inp.errAt)

  var patterns: seq[string] = @[]
  var ruleToBranch: seq[int] = @[]
  for j in 0 ..< inp.branches.len:
    for p in inp.branches[j].patterns:
      patterns.add p
      ruleToBranch.add j
  if patterns.len == 0:
    return errorTree("`" & name & "` needs at least one `of <pattern>:` branch", n)

  var dfa = default(Dfa)
  var err = ""
  # `reNoCaptures` / `reNoBackrefs`: a generated matcher walks the DFA and has
  # nowhere to record a capture, so `(x)` here means grouping. `reExtended`
  # matches what `re` does at runtime, so a pattern reads the same either way.
  let status = rulesToDfa(patterns, {reExtended, reNoCaptures, reNoBackrefs},
                          dfa, err)
  if status == TooComplex:
    return errorTree("these `" & name & "` patterns need more than " &
                     $MaxLabel & " automaton states; split them up", n)
  if err.len > 0:
    return errorTree("invalid regular expression: " & err, n)
  if dfa.stateCount < 1 or dfa.startState < 1 or dfa.startState > dfa.stateCount:
    # Not reachable from any pattern the parser accepts; stated because the
    # emitted `case` would otherwise be a `case` with no `of` branch, or one
    # whose start state matches none of them.
    return errorTree("`" & name & "` built an empty automaton", n)

  # `lex s:` has no scan position of its own, so it gets a local one and the
  # match is required to reach the end of `s`.
  let ownPos = genSym()
  var posCur = inp.pos
  let st = genSym()
  let ch = genSym()
  let lastRule = genSym()
  let lastPos = genSym()

  result = createTree()
  var genErr = ""
  result.withTree StmtsS, info:
    if not inp.scanning:
      result.withTree VarS, info:
        result.addSymDef ownPos, info
        result.addEmptyNode3 info
        result.addIntLit 0
    var posTree = createTree()
    if inp.scanning:
      posTree.addSubtree posCur
    else:
      posTree.addSymUse ownPos, info
    let pos = snapshot(posTree)

    result.addMachine dfa, inp.s, pos, st, ch, lastRule, lastPos, info, genErr

    if inp.scanning:
      # rewind to the last accepting position (`lastPos` starts at the entry
      # position, so "nothing matched" leaves `pos` exactly where it was)
      result.withTree AsgnS, info:
        result.addSubtree pos
        result.addSymUse lastPos, info
    else:
      # a partial match is not a match here
      result.withTree IfS, info:
        result.withTree ElifU, info:
          result.withTree InfixX, info:
            result.addIdent "!="
            result.addSymUse lastPos, info
            result.withTree CallX, info:
              result.bindSym "len"
              result.addSubtree inp.s
          result.withTree StmtsS, info:
            result.addAsgnInt lastRule, 0, info

    result.addDispatch inp, ruleToBranch, lastRule, info

  if genErr.len > 0:
    result = errorTree(genErr, n)

var input = loadPluginInput()
let called = pluginName(input)
if called == "re":
  saveTree transformRe(input)
else:
  saveTree transform(input, called)
