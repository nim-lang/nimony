#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

##[
Contract analysis. Tries to prove or disprove `.requires` and `.ensures`
annotations.

The analysis is performed on the control flow graph of a routine or toplevel
code snippet.

In order to not to be too annoying in the case of a contract violation, the
compiler emits a warning (that can be suppressed or turned into an error).
The warning also triggers a runtime check so this entire pass becomes yet
another code transformation.

]##

import std / [assertions]

include nifprelude

import ".." / models / tags
import nimony_model, programs, decls, typenav, sembasics, reporters, renderer, typeprops, inferle

type
  Context = object
    dest: TokenBuf
    r: CurrentRoutine
    typeCache: TypeCache
    facts: Facts
    toPropId: Table[SymId, VarId]

proc takeToken(c: var Context; n: var Cursor) {.inline.} =
  c.dest.add n
  inc n

proc takeParRi(c: var Context; n: var Cursor) =
  if n.kind == ParRi:
    c.dest.add n
    inc n
  else:
    error "expected ')', but got: ", n

#[

proc foo(x: int) {.requires: x > 0.}
foo(2)

--> We must translate the `requires: x > 0.` to `2 > 0`.
But inferle already requires us to do a translation step here.

]#

proc argAt(call: Cursor; pos: int): Cursor =
  result = call
  inc result
  for i in 0 ..< pos: skip result

type
  ProofRes = enum
    Unprovable, Proven, Disproven

proc `and`(a, b: ProofRes): ProofRes =
  if a == Unprovable or b == Unprovable:
    Unprovable
  elif a == Disproven or b == Disproven:
    Disproven
  else:
    Proven

proc `or`(a, b: ProofRes): ProofRes =
  if a == Proven or b == Proven:
    Proven
  elif a == Disproven and b == Disproven:
    Disproven
  else:
    Unprovable

proc `not`(a: ProofRes): ProofRes =
  if a == Unprovable:
    Unprovable
  elif a == Proven:
    Disproven
  else:
    Proven

proc checkReq(c: var Context; paramMap: Table[SymId, int]; req, call: Cursor): ProofRes =
  case req.kind
  of Symbol:
    let pos = paramMap.getOrDefault(req.symId)

  of ParLe:
    case req.exprKind
    of AndX:
      var r = req
      inc r
      let a = checkReq(c, paramMap, r, call)
      let b = checkReq(c, paramMap, r, call)
      skipParRi r
      result = a and b
    of OrX:
      var r = req
      inc r
      let a = checkReq(c, paramMap, r, call)
      let b = checkReq(c, paramMap, r, call)
      skipParRi r
      result = a or b
    of NotX:
      var r = req
      inc r
      result = not checkReq(c, paramMap, r, call)
      skipParRi r
    of EqX:
      # x == 3?
      var r = req
      inc r
      if r.kind == Symbol:
        let pos = paramMap.getOrDefault(r.symId)
        if pos > 0:
          let arg = call.argAt(pos)
          if arg.kind == Symbol:
            let propId = c.toPropId.getOrDefault(arg.symId)




proc analyseCallArgs(c: var Context; n: var Cursor; fnType: Cursor) =
  var fnType = skipProcTypeToParams(fnType)
  assert fnType == "params"
  inc fnType # skip `params`
  var paramMap = initTable[SymId, int]() # param to position
  while fnType.kind != ParRi:
    let param = takeLocal(fnType, SkipFinalParRi)
    paramMap[param.name.symId] = paramMap.len+1
  skipParRi fnType
  skip fnType # skip return type
  # now we have the pragmas:
  let req = extractPragma(fnType, RequiresP)
  if not cursorIsNil(req):
    # ... analyse that the input parameters match the requirements
  else:
    while n.kind != ParRi:
      skip n

proc analyseCall(c: var Context; pc: var Cursor) =
  inc pc # skip call instruction
  let fnType = skipProcTypeToParams(getType(c.typeCache, n))
  assert fnType == "params"
  analyseCallArgs(c, n, fnType)

proc singlePath(c: var Context; pc: Cursor): bool =
  var nested = 0
  var pc = pc
  var facts = createFacts()
  while true:
    #echo "PC IS: ", pc.kind
    case pc.kind
    of GotoInstr:
      let diff = pc.getInt28
      if diff < 0:
        # jump backwards:
        let back = pc +! diff
        if not testOrSetMark(back):
          pc = back
        else:
          # finished traversing this path:
          break
      else:
        # ordinary goto, simply follow it:
        pc = pc +! diff
    of ParRi:
      if nested == 0:
        raiseAssert "BUG: unpaired ')'"
      dec nested
      inc pc
    of Symbol:
      inc pc
    of SymbolDef:
      raiseAssert "BUG: symbol definition in single path"
    of EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
      inc pc
    of ParLe:
      #echo "PC IS: ", pool.tags[pc.tag]
      if pc.cfKind == IteF:
        inc pc
        if containsUsage(pc, x):
          otherUsage = pc
          return false
        # now 2 goto instructions follow:
        let a = pc +! pc.getInt28
        inc pc
        let b = pc +! pc.getInt28
        # we follow the second goto and remember the first one:
        if not isMarked(a):
          pcs.add a
        pc = b
      else:
        let kind = pc.stmtKind
        case kind
        of AsgnS:
          inc pc
          skip pc # skip left-hand-side
          skip pc # skip right-hand-side
          skipParRi pc
        of RetS:
          # check if `result` fullfills the `.ensures` contract.
          break
        of StmtsS, ScopeS, BlockS, ContinueS, BreakS:
          inc pc
          inc nested
        of LocalDecls:
          inc pc
          let name = pc.symId
          skip pc # name
          skip pc # export marker
          skip pc # pragmas
          c.typeCache.registerLocal(name, cast[SymKind](kind), pc)
          skip pc # type
          inc nested
          # proceed with its value here
        of NoStmt:
          raiseAssert "BUG: unknown statement: " & toString(pc, false)
        of DiscardS:
          inc pc
          inc nested
        of CallS, CmdS:
          analyzeCall(c, pc)
        of EmitS, InclS, ExclS:
          # not of interest for contract analysis:
          skip pc
        of IfS, WhenS, WhileS, ForS, CaseS, TryS, YldS, RaiseS, ExportS,
           IncludeS, ImportS, FromS, ImportExceptS, CommentS, PragmasS,
           ImportasS, ExportexceptS, BindS, MixinS, UsingS,
           UnpackDeclS, StaticstmtS, AsmS, DeferS:
          raiseAssert "BUG: statement not eliminated: " & $pc.stmtKind
        of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS:
          # declarative junk we don't care about:
          skip pc
  return true

proc checkContracts(c: var Context): bool =
  var n = readonlyCursorAt(c.cf, 0)
  #echo "LOOKING AT: ", codeListing(c)
  var pcs = @[n]
  while pcs.len > 0:
    let pc = pcs.pop()
    #echo "Looking at: ", toString(pc, false)
    if not isMarked(pc):
      if not singlePath(c, pc):
        return false
      doMark pc
  return true

proc analyzeContracts*(input: var TokenBuf): TokenBuf =
  let oldInfos = prepare(input)
  var c = Context(typeCache: createTypeCache(),
    dest: createTokenBuf(500),
    cf: toControlflow(beginRead input))
  freeze c.cf
  #echo "CF IS ", codeListing(c.cf)
  c.typeCache.openScope()

  discard checkContracts(c)

  endRead input
  restore(input, oldInfos)
  c.typeCache.closeScope()
  result = ensureMove(c.dest)
