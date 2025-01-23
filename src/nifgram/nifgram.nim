#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Generates code that can validate a NIF file according to a grammar file
## that is also in NIF notation.
## See nifc/nifc_grammar.nif for a real world example.

import std / [strutils, tables, sets, assertions, syncio]
import "../lib" / [stringviews, nifreader]

type
  RuleFlag = enum
    LateDecl, Overloadable, Collect
  ContextKind = enum
    Grammar, Generator
  Context = object
    r: Reader
    t: Token
    kind: ContextKind
    startRule, currentRule: string
    ruleFlags: set[RuleFlag]
    seen, used, usedBindings: HashSet[string]
    bindings: Table[string, string]
    outp, forw, locals: string
    nesting, tmpCounter, inBinding, inMatch: int
    procPrefix, signature, args0, leaveBlock: string
    declaredVar, collectInto, flipVar: string
    specTags, foundTags: OrderedTable[string, int] # maps to the arity for more checking

proc error(c: var Context; msg: string) {.noreturn.} =
  #writeStackTrace()
  quit "[Error] in RULE " & c.currentRule & "(" & $c.r.line & "): " & msg

proc ind(c: var Context) =
  c.outp.add '\n'
  for i in 1..c.nesting*2:
    c.outp.add ' '

template args(c: Context): string {.dirty.} = (if it.len > 0: c.args0 & ", " & it else: c.args0)

proc compileExpr(c: var Context; it: string): string

proc declTemp(c: var Context; prefix: string; value = "false"): string =
  inc c.tmpCounter
  result = prefix & $c.tmpCounter
  ind c
  c.outp.add "var " & result & " = " & value

proc declTempOuter(c: var Context, prefix: string; value = "false"): string =
  inc c.tmpCounter
  result = prefix & $c.tmpCounter
  c.locals.add "\n  "
  c.locals.add "var " & result & " = " & value

proc compileErr(c: var Context; it: string): string =
  c.t = next(c.r)
  if c.t.tk == StringLit:
    result = "error( " & c.args & ", " & escape(decodeStr(c.t)) & ")"
    c.t = next(c.r)
  else:
    result = ""
    error c, "string literal after ERR expected"
  if c.t.tk == ParRi:
    c.t = next(c.r)
  else:
    error c, "')' for ERR expected"

proc compileOr(c: var Context; it: string): string =
  result = if c.inMatch > 0: declTempOuter(c, "or") else: declTemp(c, "or")
  inc c.tmpCounter
  let lab = "or" & $c.tmpCounter
  c.t = next(c.r)
  ind c
  c.outp.add "block " & lab & ":"
  inc c.nesting
  let oldLeaveBlock = c.leaveBlock
  c.leaveBlock = "break " & lab

  while true:
    if c.t.tk == ParLe and c.t.s == "ERR":
      ind c
      c.outp.add compileErr(c, it)
      break

    let cond = compileExpr(c, it)
    ind c
    c.outp.add "if "
    c.outp.add cond
    c.outp.add ":"
    inc c.nesting
    ind c
    c.outp.add result
    c.outp.add " = true"
    ind c
    c.outp.add "break " & lab
    dec c.nesting
    if c.t.tk == ParRi:
      break
  dec c.nesting
  c.leaveBlock = oldLeaveBlock

proc emitForLoop(c: var Context; it: string): string =
  when false:
    result = forLoopVar(c, "ch")
    ind c
    c.outp.add "for " & result & " in sons(" & c.args & "):"
  else:
    result = it
    ind c
    c.outp.add "while not peekParRi(" & c.args & "):"

proc compileZeroOrMany(c: var Context; it: string): string =
  result = declTemp(c, "zm", "true")
  c.t = next(c.r)
  let tmp = emitForLoop(c, it)
  inc c.nesting
  let cond = compileExpr(c, tmp)
  if cond != "true":
    ind c
    c.outp.add "if not "
    c.outp.add cond
    c.outp.add ":"
    inc c.nesting
    ind c
    c.outp.add result & " = false"
    ind c
    c.outp.add "break"
    dec c.nesting
  dec c.nesting

proc compileOneOrMany(c: var Context; it: string): string =
  result = declTemp(c, "om")
  c.t = next(c.r)
  let tmp = emitForLoop(c, it)
  inc c.nesting
  let cond = compileExpr(c, tmp)
  if cond != "true":
    ind c
    c.outp.add "if not "
    c.outp.add cond
    c.outp.add ":"
    inc c.nesting
    ind c
    c.outp.add "break"
    dec c.nesting
    ind c
    c.outp.add "else:"
    inc c.nesting
    ind c
    c.outp.add result & " = true"
    dec c.nesting
  dec c.nesting

proc compileZeroOrOne(c: var Context; it: string): string =
  # XXX This is not completely correct. It must add code to backtrack
  # if `cond` failed.
  c.t = next(c.r)

  let cond = compileExpr(c, it)
  ind c
  c.outp.add "discard "
  c.outp.add cond
  result = "true"

proc upcase(s: string): string =
  toUpperAscii(s[0]) & substr(s, 1)

proc tagAsNimIdent(tag: string): string =
  upcase(tag) & "T"

proc compileKeyw(c: var Context; it: string): string =
  let tag = decodeStr(c.t)

  c.foundTags[tag] = 1
  if c.specTags.len > 0 and not c.specTags.hasKey(tag):
    error c, "unknown tag: " & tag

  let cond = "isTag(" & c.args & ", " & tagAsNimIdent(tag) & ")"
  c.t = next(c.r)
  if c.t.tk == ParRi and c.inMatch == 0:
    if c.kind == Generator:
      return "matchAndEmitTag(" & c.args & ", " & tagAsNimIdent(tag) & ", " & escape(tag) & ")"
    return cond

  result = if c.inMatch > 0: declTempOuter(c, "kw") else: declTemp(c, "kw")

  ind c
  c.outp.add "if "
  c.outp.add cond
  c.outp.add ":"

  inc c.nesting

  var firstArg = c.kind == Generator
  while true:
    if c.t.tk == ParRi: break

    if firstArg:
      firstArg = false
      if c.t.tk != StringLit:
        # implicit "emit" rule: for `(tag Expr Expr)` produce
        # `emit("tag")`:
        ind c
        c.outp.add "emitTag(" & c.args & ", " & escape(tag) & ")"

    let e = c.t

    let cond = compileExpr(c, it)
    if cond != "true":
      ind c
      c.outp.add "if not "
      c.outp.add cond
      c.outp.add ":"
      inc c.nesting
      var errmsg = decodeStr(e)
      if errmsg == ".":
        errmsg = "in rule " & c.currentRule & ": <empty node> expected"
      elif errmsg in ["ZERO_OR_MANY", "ONE_OR_MANY", "OR", "ZERO_OR_ONE",
                      "SCOPE", "ENTER", "QUERY", "COND", "DO", "LET",
                      "MATCH"]:
        errmsg = "invalid " & c.currentRule
      else:
        errmsg.add " expected"
      if c.inMatch == 0:
        ind c
        c.outp.add "error(" & c.args & ", " & escape(errmsg) & ")"
      else:
        ind c
        c.outp.add c.leaveBlock
      dec c.nesting

  ind c
  c.outp.add result
  c.outp.add " = matchParRi(" & c.args & ")"

  dec c.nesting
  if c.inMatch == 0:
    discard
  else:
    ind c
    c.outp.add "else: "
    c.outp.add c.leaveBlock

proc compileRuleInvokation(c: var Context; it: string): string =
  let ruleName = decodeStr(c.t)
  if not c.seen.contains(ruleName):
    if not c.used.containsOrIncl(ruleName):
      c.forw.add "proc " & c.procPrefix & ruleName & c.signature & "\n"
  else:
    c.used.incl ruleName
  result = c.procPrefix & ruleName & "(" & c.args & ")"
  if c.inBinding > 0:
    result = declTemp(c, "m", result)

proc compileSymbolDef(c: var Context; it: string): string =
  let declProc =
    if LateDecl in c.ruleFlags:
      "handleSymDef"
    elif Overloadable in c.ruleFlags:
      "declareOverloadableSym"
    else:
      "declareSym"
  c.declaredVar = declTemp(c, "sym", declProc & "(" & c.args & ")")
  result = "success(" & c.declaredVar & ")"

proc compileEmit(c: var Context; it: string): string =
  assert c.t.tk == StringLit
  let s = decodeStr(c.t)
  ind c
  c.outp.add "emit(" & c.args & ", " & escape(s) & ")"
  result = "true"

proc compileAtom(c: var Context; it: string): string =
  if c.collectInto.len > 0:
    ind c
    c.outp.add c.collectInto
    c.outp.add ".add "
    c.outp.add "save(" & c.args & ")"

  if c.t.tk == DotToken:
    result = "matchEmpty(" & c.args & ")"
  elif c.t.tk == Ident:
    if c.t.s == "SYMBOL":
      result = "lookupSym(" & c.args & ")"
    elif c.t.s == "SYMBOLDEF":
      result = compileSymbolDef(c, it)
    elif c.t.s == "IDENT":
      result = "matchIdent(" & c.args & ")"
    elif c.t.s == "STRINGLITERAL":
      result = "matchStringLit(" & c.args & ")"
    elif c.t.s == "CHARLITERAL":
      result = "matchCharLit(" & c.args & ")"
    elif c.t.s == "INTLIT":
      result = "matchIntLit(" & c.args & ")"
    elif c.t.s == "UINTLIT":
      result = "matchUIntLit(" & c.args & ")"
    elif c.t.s == "FLOATLIT":
      result = "matchFloatLit(" & c.args & ")"
    elif c.t.s == "ANY":
      result = "matchAny(" & c.args & ")"
    else:
      result = compileRuleInvokation(c, it)
  elif c.kind == Generator and c.t.tk == StringLit:
    result = compileEmit(c, it)
  else:
    result = ""
    error c, "IDENT expected but got " & $c.t

proc compileIdent(c: var Context; it: string): string =
  c.t = next(c.r)
  if c.t.tk == StringLit:
    result = "matchIdent(" & c.args & ", " & escape(decodeStr(c.t)) & ")"
    c.t = next(c.r)
  else:
    result = ""
    error c, "string literal after IDENT expected"

proc compileQuery(c: var Context; it, prefix: string): string =
  c.t = next(c.r)
  result = ""
  while c.t.tk in {StringLit, Ident}:
    if result.len > 0: result.add " or "
    result.add prefix & decodeStr(c.t) & "(" & c.args & ")"
    c.t = next(c.r)

  if c.t.tk != ParRi:
    result = ""
    error c, "string literal after QUERY|COND expected"

proc compileDo(c: var Context; it: string): string =
  c.t = next(c.r)

  if c.t.tk in {StringLit, Ident}:
    ind c
    c.outp.add decodeStr(c.t) & "(" & c.args0
    c.t = next(c.r)
  else:
    error c, "string literal after DO expected"

  var counter = 1
  while c.t.tk in {StringLit, Ident}:
    if counter > 0: c.outp.add ", "
    let s = decodeStr(c.t)
    if c.t.tk == Ident:
      let asNimIdent = c.bindings.getOrDefault(s)
      if asNimIdent.len > 0:
        c.outp.add asNimIdent
        c.usedBindings.incl s
      else:
        error c, "undeclared binding: " & s
    else:
      c.outp.add s
    c.t = next(c.r)
    inc counter
  c.outp.add ")"
  if c.t.tk != ParRi:
    error c, "string literal after DO expected"
  result = "true"

proc compileConcat(c: var Context; it: string) =
  while true:
    let cond = compileExpr(c, it)
    ind c
    if cond != "true":
      c.outp.add "if not "
      c.outp.add cond
      c.outp.add ": "
      c.outp.add c.leaveBlock
    if c.t.tk == ParRi: break

proc compileScope(c: var Context; it: string): string =
  c.t = next(c.r)
  ind c
  c.outp.add "openScope(c)"
  ind c
  c.outp.add "try:"
  inc c.nesting
  compileConcat c, it
  dec c.nesting
  ind c
  c.outp.add "finally:"
  inc c.nesting
  ind c
  c.outp.add "closeScope(c)"
  dec c.nesting
  result = "true"

proc compileEnter(c: var Context; it: string): string =
  c.t = next(c.r)

  var op = ""
  if c.t.tk == StringLit:
    op = decodeStr(c.t)
    c.t = next(c.r)
  else:
    result = ""
    error c, "string literal after ENTER expected"

  ind c
  c.outp.add "enter" & op & "(" & c.args & ")"
  ind c
  c.outp.add "try:"
  inc c.nesting
  compileConcat c, it
  dec c.nesting
  ind c
  c.outp.add "finally:"
  inc c.nesting
  ind c
  c.outp.add "leave" & op & "(" & c.args0 & ")"
  dec c.nesting
  result = "true"

proc compileFlipFlop(c: var Context; it, mode: string): string =
  c.t = next(c.r)

  var op = ""
  if c.t.tk == StringLit:
    op = decodeStr(c.t)
    c.t = next(c.r)
  else:
    error c, "string literal after FLIP|FLOP expected"

  let isFlip = mode == "flip"
  if isFlip:
    ind c
    result = declTemp(c, "valid")
  else:
    result = "true"

  ind c
  let flp = declTemp(c, "flp", "push" & op & "(" & c.args &
                    (if isFlip: (", " & result) else: "") & ")")

  if c.flipVar.len == 0:
    c.flipVar = flp

  if isFlip:
    ind c
    c.outp.add "if " & result & ":"
    inc c.nesting

  ind c
  c.outp.add "try:"
  inc c.nesting
  compileConcat c, it
  dec c.nesting
  ind c
  c.outp.add "finally:"
  inc c.nesting
  ind c
  c.outp.add "pop" & op & "(" & c.args0 & ", " & flp & ")"
  dec c.nesting
  if isFlip:
    dec c.nesting

proc compileLet(c: var Context; it: string): string =
  c.t = next(c.r)

  var key = ""
  if c.t.tk == SymbolDef:
    key = decodeStr(c.t)
    c.t = next(c.r)
  else:
    result = ""
    error c, ":SYMBOLDEF after LET expected"
  inc c.tmpCounter
  let v = key & $c.tmpCounter
  c.bindings[key] = v

  c.locals.add "\n  var " & v & ": Binding"
  ind c
  c.outp.add v & " = startBinding" & "(" & c.args & ")"

  inc c.inBinding
  result = compileExpr(c, it)
  dec c.inBinding

  ind c
  c.outp.add "finishBinding" & "(" & c.args & ", " & v & ")"

proc compileMatch(c: var Context; it: string): string =
  c.t = next(c.r)
  let oldLeaveBlock = c.leaveBlock

  inc c.inMatch

  inc c.tmpCounter
  let lab = "m" & $c.tmpCounter

  let before = declTemp(c, "before", "save(" & c.args & ")")

  ind c
  c.outp.add "block " & lab & ":"
  c.leaveBlock = "(; rollback(" & c.args0 & ", " & before & "); break " & lab & ")"

  inc c.nesting

  result = compileExpr(c, it)
  let actions = compileExpr(c, it)
  assert actions == "true"

  dec c.nesting
  dec c.inMatch
  c.leaveBlock = oldLeaveBlock

proc compileExpr(c: var Context; it: string): string =
  if c.t.tk == ParLe:
    if c.t.s == "OR":
      result = compileOr(c, it)
    elif c.t.s == "ZERO_OR_MANY":
      result = compileZeroOrMany(c, it)
    elif c.t.s == "ONE_OR_MANY":
      result = compileOneOrMany(c, it)
    elif c.t.s == "ZERO_OR_ONE":
      result = compileZeroOrOne(c, it)
    elif c.t.s == "SCOPE":
      result = compileScope(c, it)
    elif c.t.s == "IDENT":
      result = compileIdent(c, it)
    elif c.t.s == "QUERY":
      result = compileQuery(c, it, "query")
    elif c.t.s == "COND":
      result = compileQuery(c, it, "")
    elif c.t.s == "DO":
      result = compileDo(c, it)
    elif c.t.s == "ENTER":
      result = compileEnter(c, it)
    elif c.t.s == "FLIP":
      result = compileFlipFlop(c, it, "flip")
    elif c.t.s == "FLOP":
      result = compileFlipFlop(c, it, "flop")
    elif c.t.s == "LET":
      result = compileLet(c, it)
    elif c.t.s == "MATCH":
      result = compileMatch(c, it)
    else:
      result = compileKeyw(c, it)
    if c.t.tk == ParRi:
      c.t = next(c.r)
    else:
      result = ""
      error c, "')' expected but got " & $c.t
  else:
    result = compileAtom(c, it)
    c.t = next(c.r)

proc compileRule(c: var Context; it: string) =
  c.t = next(c.r)
  if c.t.tk == SymbolDef:
    c.tmpCounter = 0
    c.currentRule = $c.t.s
    if containsOrIncl(c.seen, c.currentRule):
      error c, "attempt to redeclare RULE named " & c.currentRule
    ind c
    ind c
    c.outp.add "proc " & c.procPrefix & c.currentRule & c.signature & " ="
    inc c.nesting
    let oldOutp = move(c.outp)
    c.t = next(c.r)

    c.ruleFlags = {}
    c.declaredVar = ""
    c.locals = ""
    c.collectInto = ""
    c.flipVar = ""

    while c.t.tk == Ident:
      if c.t.s == "LATEDECL":
        c.ruleFlags.incl LateDecl
        c.t = next(c.r)
      elif c.t.s == "OVERLOADABLE":
        c.ruleFlags.incl Overloadable
        c.t = next(c.r)
      elif c.t.s == "COLLECT":
        c.ruleFlags.incl Collect
        c.t = next(c.r)
      else:
        break

    let action = "handle" & upcase(c.currentRule)
    var before = ""
    if Collect in c.ruleFlags:
      before = declTemp(c, "before", "@[save(" & c.args & ")]")
      c.collectInto = before
    else:
      ind c
      c.outp.add "when declared("
      c.outp.add action
      c.outp.add "):"
      inc c.nesting
      before = declTemp(c, "before", "save(" & c.args & ")")
      dec c.nesting

    compileConcat c, it

    if LateDecl in c.ruleFlags:
      if c.declaredVar.len == 0:
        error c, "LATEDECL used but no SYMBOLDEF in rule found"
      else:
        let declProc =
          if Overloadable in c.ruleFlags: "addOverloadableSym"
          else: "addSym"
        ind c
        c.outp.add declProc & "(" & c.args0 & ", " & c.declaredVar & ")"

    let moreArgs = before & (if c.flipVar.len > 0: (", " & c.flipVar) else: "")
    if Collect in c.ruleFlags:
      ind c
      c.outp.add action & "(" & c.args & ", " & moreArgs & ")"
    else:
      ind c
      c.outp.add "when declared("
      c.outp.add action
      c.outp.add "):"
      inc c.nesting
      ind c
      c.outp.add action & "(" & c.args & ", " & moreArgs & ")"
      dec c.nesting

    ind c
    c.outp.add "return true"
    dec c.nesting

    for k, _ in pairs(c.bindings):
      if not c.usedBindings.contains(k):
        error c, "unused binding: " & k
    c.bindings.clear()

    let body = move(c.outp)

    c.outp = ensureMove(oldOutp)
    c.outp.add c.locals
    c.outp.add body

    if c.t.tk == ParRi:
      c.t = next(c.r)
    else:
      error c, "')' expected, but got " & $c.t
  else:
    error c, "SymbolDef expected, but got " & $c.t

proc compile(c: var Context) =
  c.t = next(c.r)
  if c.t.tk == ParLe and (c.t.s == "GRAMMAR" or c.t.s == "GENERATOR"):
    if c.t.s == "GENERATOR":
      c.kind = Generator
      c.signature = "(c: var Context): bool"
      c.procPrefix = "gen"

    c.t = next(c.r)
    if c.t.tk == Ident:
      c.startRule = $c.t.s
      c.t = next(c.r)
    else:
      error c, "GRAMMAR takes an IDENT that is the name of the starting rule"
    while true:
      if c.t.tk == ParLe and c.t.s == "RULE":
        compileRule(c, (if c.kind == Generator: "" else: "it"))
      elif c.t.tk == ParLe and c.t.s == "COM":
        # skip comment:
        var nested = 1
        while true:
          c.t = next(c.r)
          if c.t.tk == EofToken:
            error c, "')' expected, but got " & $c.t
          if c.t.tk == ParLe: inc nested
          elif c.t.tk == ParRi:
            dec nested
            if nested == 0:
              c.t = next(c.r)
              break
      else:
        break
    if c.t.tk == ParRi:
      c.t = next(c.r)
    else:
      error c, "')' expected, but got " & $c.t
  else:
    error c, "GRAMMAR expected but got " & $c.t

proc main(inp, outp: string;
          specTags: sink OrderedTable[string, int]): OrderedTable[string, int] =
  var r = nifreader.open(inp)
  discard processDirectives(r)
  var c = Context(r: r, procPrefix: "match", signature: "(c: var Context; it: var Item): bool",
                  args0: "c", leaveBlock: "return false", specTags: ensureMove(specTags))
  c.compile
  c.r.close
  if outp.len > 0:
    var foutp = open(outp, fmWrite)
    writeLine foutp, "# GENERATED BY NifGram. DO NOT EDIT!"
    writeLine foutp, "# ORIGINAL FILE: " & (when defined(windows): inp.replace('\\', '/') else: inp)
    writeLine foutp, c.forw
    writeLine foutp, c.outp
    foutp.close()
  for d in c.used:
    if not c.seen.contains(d):
      error c, "undeclared rule: " & d

  for d in c.seen:
    if not c.used.contains(d) and d != c.startRule:
      error c, "unused rule: " & d
  result = ensureMove(c.foundTags)

const
  Version = "0.6"
  Usage = "NifGram. Version " & Version & """

  (c) 2024 Andreas Rumpf
Usage:
  nifgram [options] [command] [arguments]
Command:
  file.nif [output.nim]     convert the NIF grammar to Nim

Options:
  --tags:file.nim       update file.nim with the used tags
  --tagsfrom:file.nif   extract the tags from the NIF file as
                        the list of existing tags
  --version             show the version
  --help                show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

when isMainModule:
  import std / [parseopt, os]

  proc handleCmdLine() =
    var inp = ""
    var outp = ""
    var tagsFile = ""
    var specTags: OrderedTable[string, int] = initOrderedTable[string, int]()
    for kind, key, val in getopt():
      case kind
      of cmdArgument:
        if inp.len == 0:
          inp = key
        elif outp.len == 0:
          outp = key
        else:
          quit "FATAL: Too many arguments"
      of cmdLongOption, cmdShortOption:
        case normalize(key)
        of "help", "h": writeHelp()
        of "version", "v": writeVersion()
        of "tagsfrom": specTags = main(val, "", initOrderedTable[string, int]())
        of "tags": tagsFile = val
        else: writeHelp()
      of cmdEnd: assert false, "cannot happen"
    let foundTags = main(inp, (if outp.len > 0: outp else: changeFileExt(inp, ".nim")), specTags)
    if tagsFile.len > 0 and foundTags.len > 0:
      var t = open(tagsFile, fmWrite)
      t.writeLine "# GENERATED BY NifGram. DO NOT EDIT!"
      t.writeLine "# ORIGINAL FILE: " & (when defined(windows): inp.replace('\\', '/') else: inp)

      t.writeLine("import nifstreams")
      t.writeLine("\nconst")
      var i = 3
      try:
        for tag, _ in pairs foundTags:
          t.writeLine "  ", tagAsNimIdent(tag), "* = TagId(", i, ")"
          inc i

        i = 3
        t.writeLine("\nproc registerTags*() =")
        for tag, _ in pairs foundTags:
          t.writeLine "  registerTag ", escape(tag), ", ", tagAsNimIdent(tag)
          inc i

      finally:
        t.close()

  handleCmdLine()
