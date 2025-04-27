#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Generates code that can validate a NIF file according to a grammar file
## that is also in NIF notation.
## See nifc/nifc_grammar.nif for a real world example.

import std / [strutils, tables, sets, assertions, syncio, deques, sequtils]
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
    seenRules, used, usedBindings: HashSet[string]
    bindings: Table[string, string]
    outp, forw, locals: string
    nesting, tmpCounter, inBinding, inMatch, inStack: int
    popVars: Table[string, HashSet[string]]
    localPopVars: seq[string]
    localPopCounts: seq[int] # pop vars in current exprs nesting level

    procPrefix, args0, leaveBlock: string
    declaredVar, collectInto, flipVar: string
    specTags, foundTags: OrderedTable[string, int] # maps to the arity for more checking

  ScanContext = object
    r: Reader
    t: Token
    currentRule: string

    invocations: Table[string, HashSet[string]] # use pop var or rule invocation
    ruleInvocations: Table[string, HashSet[string]] # rule invocations in rule
    popVars: Table[string, HashSet[string]] # required pop vars to use rule
    
    rules: seq[string] # all declared rules

proc normalizedPopVar(popVar: string): string = 
  result = "st"
  result.add nimIdentNormalize(popVar)

proc signature(c: Context, rule: string): string {.inline.} = 
  if c.kind == Generator:
    result = ""
    result.add "(c: var Context"
    for popVar in c.popVars.getOrDefault(rule):
      result.add ", "
      result.add normalizedPopVar(popVar)
      result.add ": string"
    result.add "): bool"
  else:
    result = "(c: var Context; it: var Item): bool"

proc error(c: var Context; msg: string) {.noreturn.} =
  #writeStackTrace()
  quit "[Error] in RULE " & c.currentRule & "(" & $c.r.line & "): " & msg

proc error(c: var ScanContext; msg: string) {.noreturn.} =
  #writeStackTrace()
  quit "[Error] in RULE " & c.currentRule & "(" & $c.r.line & "): " & msg

proc skipComment[T: Context or ScanContext](c: var T) =
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
  
  var stackSaveVar = ""
  let needsStackSave = c.localPopVars.len > 0 or c.popVars[c.currentRule].len > 0
  if needsStackSave:
    stackSaveVar =
      if c.inMatch > 0: declTempOuter(c, "st", "getStack(" & c.args0 & ")")
      else: declTemp(c, "st", "getStack(" & c.args0 & ")")
  
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
    if needsStackSave:
      ind c
      c.outp.add "restoreStack(" & c.args0 & ", " & stackSaveVar & ")"
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

proc compileKeywArgs(c: var Context; it, tag, resultVar: string) =
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
  c.outp.add resultVar
  c.outp.add " = matchParRi(" & c.args & ")"

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
  compileKeywArgs(c, it, tag, result)
  dec c.nesting
  if c.inMatch == 0:
    discard
  else:
    ind c
    c.outp.add "else: "
    c.outp.add c.leaveBlock

proc compilePopVar(c: var Context; it: string): string =
  ind c
  c.outp.add "emit(" & c.args0 & ", " & normalizedPopVar($c.t.s) & ")"
  result = "true"

proc compileRuleInvocation(c: var Context; it: string): string =
  let ruleName = decodeStr(c.t)
  if not c.seenRules.contains(ruleName):
    if not c.used.containsOrIncl(ruleName):
      c.forw.add "proc " & c.procPrefix & ruleName & c.signature(ruleName) & "\n"
  else:
    c.used.incl ruleName
  
  result = c.procPrefix & ruleName & "(" & c.args
  for popVar in c.popVars[ruleName]:
    result.add ", "
    result.add normalizedPopVar(popVar)
  result.add ")"
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
    elif $c.t.s in c.popVars.getOrDefault(c.currentRule) or $c.t.s in c.localPopVars:
      result = compilePopVar(c, it)
    else:
      result = compileRuleInvocation(c, it)
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

proc compileStack(c: var Context; it: string): string =
  c.t = next(c.r)
  ind c
  c.outp.add "startStack(" & c.args & ")"
  inc c.inStack
  result = compileExpr(c, it)
  dec c.inStack
  ind c
  c.outp.add "endStack(" & c.args & ")"
  result = "true"

proc compilePop(c: var Context; it: string): string =
  c.t = next(c.r)

  var varName = ""
  if c.t.tk == SymbolDef:
    varName = decodeStr(c.t)
    c.t = next(c.r)
  else:
    result = ""
    error c, ":SYMBOLDEF after POP expected"
  
  c.localPopVars.add varName
  inc c.localPopCounts[^1]

  ind c
  c.outp.add "var " & normalizedPopVar(varName) & " = popStack(" & c.args & ")"
  result = "true"

proc compileExpr(c: var Context; it: string): string =
  if c.t.tk == ParLe:
    c.localPopCounts.add 0
    let op = $c.t.s
    case op
    of "OR":
      result = compileOr(c, it)
    of "ZERO_OR_MANY":
      result = compileZeroOrMany(c, it)
    of "ONE_OR_MANY":
      result = compileOneOrMany(c, it)
    of "ZERO_OR_ONE":
      result = compileZeroOrOne(c, it)
    of "SCOPE":
      result = compileScope(c, it)
    of "IDENT":
      result = compileIdent(c, it)
    of "QUERY":
      result = compileQuery(c, it, "query")
    of "COND":
      result = compileQuery(c, it, "")
    of "DO":
      result = compileDo(c, it)
    of "ENTER":
      result = compileEnter(c, it)
    of "FLIP":
      result = compileFlipFlop(c, it, "flip")
    of "FLOP":
      result = compileFlipFlop(c, it, "flop")
    of "LET":
      result = compileLet(c, it)
    of "MATCH":
      result = compileMatch(c, it)
    of "STACK":
      result = compileStack(c, it)
    of "POP":
      result = compilePop(c, it)
    else:
      result = compileKeyw(c, it)

    c.t = next(c.r)

    if op != "POP":
      c.localPopVars.shrink(c.localPopVars.len - c.localPopCounts.pop())
  else:
    result = compileAtom(c, it)
    c.t = next(c.r)

proc compileRule(c: var Context; it: string) =
  c.t = next(c.r)
  c.tmpCounter = 0
  c.currentRule = $c.t.s
  c.localPopCounts = @[0]
  c.localPopVars = @[]
  c.seenRules.incl c.currentRule
  ind c
  ind c
  c.outp.add "proc " & c.procPrefix & c.currentRule & c.signature(c.currentRule) & " ="
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

  c.t = next(c.r) # skip ParRi

proc compile(c: var Context) =
  c.t = next(c.r)
  if c.t.s == "GENERATOR":
    c.kind = Generator
    c.procPrefix = "gen"

  c.t = next(c.r)
  c.startRule = $c.t.s
  c.t = next(c.r) # skip ident

  while true:
    if c.t.tk == ParLe and c.t.s == "RULE":
      compileRule(c, (if c.kind == Generator: "" else: "it"))
    elif c.t.tk == ParLe and c.t.s == "COM":
      c.skipComment()
    else:
      break
  c.t = next(c.r)

type
  PeaSccContext = object
    rindex: Table[string, int]
    index: int
    c: int
    vertexStack: seq[string]
    indexStack: seq[int]
    root: Table[string, bool]
    s: seq[string] # in some implementations it used with only with vS but it incorrect
                   # (https://github.com/php/php-src/pull/12528#issue-1963500321)
    
    ruleInvocations {.requiresInit.}: Table[string, seq[string]] # E(v)

proc beginVisiting(c: var PeaSccContext, rule: string) =
  c.vertexStack.add rule
  c.indexStack.add 0
  c.root[rule] = true
  c.rindex[rule] = c.index
  inc c.index

proc finishEdge(c: var PeaSccContext, rule: string, k: int) =
  let w = c.ruleInvocations[rule][k]
  if c.rindex[w] < c.rindex[rule]:
    c.rindex[rule] = c.rindex[w]
    c.root[rule] = false

proc beginEdge(c: var PeaSccContext, rule: string, k: int): bool =
  let w = c.ruleInvocations[rule][k]
  if c.rindex[w] == 0:
    c.indexStack[^1] = k + 1
    c.beginVisiting(w)
    true
  else:
    false

proc finishVisiting(c: var PeaSccContext, rule: string) =
  discard c.vertexStack.pop()
  discard c.indexStack.pop()
  if c.root[rule]:
    dec c.index
    while c.s.len > 0 and c.rindex[rule] <= c.rindex[c.s[^1]]:
      let w = c.s.pop()
      c.rindex[w] = c.c
      dec c.index
    c.rindex[rule] = c.c
    dec c.c
  else:
    c.s.add rule

proc visitLoop(c: var PeaSccContext) =
  let rule = c.vertexStack[^1]
  var i = c.indexStack[^1]
  let verticesCnt = len(c.ruleInvocations[rule])
  while i <= verticesCnt:
    if i > 0: c.finishEdge(rule, i-1)
    if i < verticesCnt and c.beginEdge(rule, i): return
    inc i
  c.finishVisiting(rule)

proc visit(c: var PeaSccContext, rule: string) =
  c.beginVisiting(rule)
  while c.vertexStack.len > 0:
    c.visitLoop()

proc findSccs(c: ScanContext): Table[string, int] =
  # find SCCs using Pearce's algorithm
  # see https://www.sciencedirect.com/science/article/abs/pii/S0020019015001532
  #     https://www.timl.id.au/scc
  # 
  var invocations = initTable[string, seq[string]]() # stupid O(n)
  for key, value in c.ruleInvocations:
    invocations[key] = value.toSeq
  
  var s = PeaSccContext(ruleInvocations: invocations)
  for rule in invocations.keys:
    s.rindex[rule] = 0
  
  s.index = 1
  s.c = invocations.len - 1
  for rule in invocations.keys:
    if s.rindex[rule] == 0:
      s.visit(rule)
  s.rindex

proc scanPop(c: var ScanContext, popVars: var seq[string], popCounts: var seq[int]) =
  c.t = next(c.r)
  var varName = ""
  if c.t.tk == SymbolDef:
    varName = decodeStr(c.t)
    c.t = next(c.r)
  else:
    error c, ":SYMBOLDEF after POP expected"
  
  popVars.add varName
  inc popCounts[^1]
  
const
  atoms = [
    "SYMBOL", "SYMBOLDEF", "IDENT", "STRINGLITERAL",
    "CHARLITERAL", "INTLIT", "UINTLIT", "FLOATLIT", "ANY"
  ]

proc scanRule(c: var ScanContext) =
  c.t = next(c.r)
  
  if c.t.tk != SymbolDef:
    error c, "SymbolDef expected, but got " & $c.t
  c.currentRule = $c.t.s
  if c.currentRule in c.rules:
    error c, "attempt to redeclare RULE named " & c.currentRule
  c.rules.add c.currentRule
  c.ruleInvocations[c.currentRule] = initHashSet[string]()
  while c.t.tk == Ident:
    case $c.t.s
    of "LATEDECL", "OVERLOADABLE", "COLLECT":
      c.t = next(c.r)
    else: break
  
  # Expression: ( -> start tree, ) end tree
  var nesting = 1
  var popVars: seq[string] = @[]
  var popCounts = @[0] # RULE
  while nesting > 0:
    c.t = next(c.r)
    if c.t.tk == ParLe:
      popCounts.add 0
      if c.t.s == "POP":
        c.scanPop(popVars, popCounts)
        continue # skip ')' by next iteration so not increase nesting
      else: inc nesting
    elif c.t.tk == Ident and $c.t.s notin atoms:
      mgetOrPut(c.popVars, $c.t.s).incl popVars.toHashSet
      mgetOrPut(c.invocations, c.currentRule).incl $c.t.s
    if c.t.tk == ParRi:
      popVars.shrink(popVars.len - popCounts.pop())
      dec nesting
    
  if c.t.tk == ParRi:
    c.t = next(c.r)
  else:
    error c, "')' expected, but got " & $c.t

proc scan(c: var ScanContext) =
  # This pass is necessary to determine the arguments of the rules
  # (they differ from the standard ones if POP Var was used and then the rule was called). 
  c.t = next(c.r)
  if c.t.tk == ParLe and (c.t.s == "GRAMMAR" or c.t.s == "GENERATOR"):
    c.t = next(c.r)
    if c.t.tk == Ident:
      c.t = next(c.r)
    else:
      error c, "GRAMMAR takes an IDENT that is the name of the starting rule"
    
    while true:
      if c.t.tk == ParLe and c.t.s == "RULE":
        c.scanRule()
      elif c.t.tk == ParLe and c.t.s == "COM":
        c.skipComment()
      else:
        break
    if c.t.tk == ParRi:
      c.t = next(c.r)
    else:
      error c, "')' expected, but got " & $c.t
  else:
    error c, "GRAMMAR expected but got " & $c.t

  for rule, invocations in c.invocations.pairs:
    for invoked in invocations:
      if invoked in c.rules:
        c.ruleInvocations[rule].incl invoked
  
  let sccs = findSccs(c)
  # Pop vars in same SCC's is same
  # SCC's sorted in topological order so the bigger scc, fewer rules it invokes
  var sccRules = initTable[int, seq[string]]()
  var sccPopVars =  initTable[int, HashSet[string]]()
  for rule, scc in sccs.pairs:
    mgetOrPut(sccPopVars, scc).incl c.popVars.getOrDefault(rule) # get maximum declared pop var number
    mgetOrPut(sccRules, scc).add rule

    for invokedRule in c.ruleInvocations[rule]:
      mgetOrPut(sccPopVars, sccs[invokedRule]).incl sccPopVars[scc]
  
  for scc in 0..<c.rules.len: # only N times
    if scc in sccPopVars:
      for rule in sccRules[scc]:
        mgetOrPut(c.popVars, rule).incl sccPopVars[scc]
    

proc main(inp, outp: string;
          specTags: sink OrderedTable[string, int]): OrderedTable[string, int] =
  var r = nifreader.open(inp)
  discard processDirectives(r)
  var c = Context(r: r, procPrefix: "match",
                  args0: "c", leaveBlock: "return false", specTags: ensureMove(specTags))
  var sc = ScanContext(r: r)
  sc.scan
  c.popVars = sc.popVars
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
    if not c.seenRules.contains(d):
      error c, "undeclared rule: " & d

  for d in c.seenRules:
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
      t.writeLine("\nvar")
      try:
        for tag, _ in pairs foundTags:
          t.writeLine "  ", tagAsNimIdent(tag), "*: TagId"

        t.writeLine("\nproc registerTags*() =")
        for tag, _ in pairs foundTags:
          t.writeLine "  " & tagAsNimIdent(tag) & " = registerTag ", escape(tag)

      finally:
        t.close()

  handleCmdLine()
