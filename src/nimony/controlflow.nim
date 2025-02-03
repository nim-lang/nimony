#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Helper to translate control flow into `goto` based code
## which can be easier to analyze, depending on the used algorithm.

import std/[assertions, intsets]
include nifprelude

import nimony_model, sembasics, typenav

const
  GotoInstr = InlineInt

type
  Label = distinct int
  TempVar = distinct int
  BlockKind = enum
    IsRoutine
    IsLoop
    IsBlock
    IsTryStmt
    IsFinally
  BlockOrLoop {.acyclic.} = ref object
    kind: BlockKind
    sym: SymId # block label or for a routine its `result` symbol
    parent: BlockOrLoop
    breakInstrs: seq[Label]
    contInstrs: seq[Label]
  ControlFlow = object
    dest: TokenBuf
    stmtBegin: int
    nextVar: int
    currentBlock: BlockOrLoop
    typeCache: TypeCache

proc codeListing(c: TokenBuf, start = 0; last = -1): string =
  # for debugging purposes
  # first iteration: compute all necessary labels:
  var jumpTargets = initIntSet()
  let last = if last < 0: c.len-1 else: min(last, c.len-1)
  for i in start..last:
    if c[i].kind == GotoInstr:
      jumpTargets.incl(i+c[i].getInt32)
  # second iteration: generate string representation:
  var i = start
  var b = nifbuilder.open(1000)
  while i <= last:
    if i in jumpTargets:
      b.addTree "lab"
      b.addSymbolDef("L" & $i)
      b.endTree()
    case c[i].kind
    of GotoInstr:
      b.addTree "goto"
      b.addIdent "L" & $(i+c[i].getInt32())
      b.endTree()
    of Symbol:
      b.addSymbol pool.syms[c[i].symId]
    of SymbolDef:
      b.addSymbolDef pool.syms[c[i].symId]
    of EofToken:
      b.addRaw "\n<unexptected EOF>\n"
    of DotToken: b.addEmpty
    of Ident: b.addIdent pool.strings[c[i].litId]
    of StringLit: b.addStrLit pool.strings[c[i].litId]
    of CharLit: b.addCharLit c[i].charLit
    of IntLit: b.addIntLit pool.integers[c[i].intId]
    of UIntLit: b.addUIntLit pool.uintegers[c[i].uintId]
    of FloatLit: b.addFloatLit pool.floats[c[i].floatId]
    of ParLe: b.addTree pool.tags[c[i].tagId]
    of ParRi: b.endTree()
    inc i
  if i in jumpTargets: b.addRaw("L" & $i & ": End\n")
  result = b.extract()

proc genLabel(c: ControlFlow): Label = Label(c.dest.len)

proc jmpBack(c: var ControlFlow, p: Label; info: PackedLineInfo) =
  c.dest.add int32Token(p.int32 - c.dest.len.int32, info)

proc jmpForw(c: var ControlFlow; info: PackedLineInfo): Label =
  result = Label(c.dest.len)
  c.dest.add int32Token(0, info) # destination will be patched later

proc patch(c: var ControlFlow; p: Label) =
  # patch with current index
  c.dest[p.int].patchInt32Token int32(c.dest.len - p.int)

proc trExpr(c: var ControlFlow; n: var Cursor)
proc trStmt(c: var ControlFlow; n: var Cursor)

type
  FixupList = seq[Label]

proc trCondOp2(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  # Handles the `b` part of `a and b` or `a or b`. Simply translates it
  # to `(ite b tjmp fjmp)`.
  let info = n.info
  c.dest.addParLe(IteF, info)
  trExpr c, n # second condition
  tjmp.add c.jmpForw(info)
  fjmp.add c.jmpForw(info)
  c.dest.addParRi()
  skipParRi n

proc trAnd(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  # (ite (first-condition) L1 fjmp)
  # (lab :L1) (ite second-condition tjmp fjmp)
  let info = n.info
  c.dest.addParLe(IteF, info)
  inc n
  trExpr c, n # first-condition
  let l1 = c.jmpForw(info)
  fjmp.add c.jmpForw(info)
  c.dest.addParRi()
  c.patch l1
  trCondOp2 c, n, tjmp, fjmp

proc trOr(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  # (ite (first-condition) tjmp L1)
  # (lab :L1) (ite second-condition tjmp fjmp)
  let info = n.info
  c.dest.addParLe(IteF, info)
  inc n
  trExpr c, n # first-condition
  tjmp.add c.jmpForw(info)
  let l1 = c.jmpForw(info)
  c.dest.addParRi()
  c.patch l1
  trCondOp2 c, n, tjmp, fjmp

proc trIte(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  case n.exprKind
  of AndX:
    trAnd(c, n, tjmp, fjmp)
  of OrX:
    trOr(c, n, tjmp, fjmp)
  of NotX:
    # reverse the jump targets:
    inc n
    trIte c, n, fjmp, tjmp
    skipParRi n
  of ParX:
    inc n
    trIte c, n, tjmp, fjmp
    skipParRi n
  else:
    # cannot exploit a special case here:
    let info = n.info
    c.dest.addParLe(IteF, info)
    trExpr c, n
    tjmp.add c.jmpForw(info)
    fjmp.add c.jmpForw(info)
    c.dest.addParRi()

proc defineTemp(c: var ControlFlow; tmp: TempVar; info: PackedLineInfo) =
  c.dest.addSymDef pool.syms.getOrIncl("`cf." & $int(tmp)), info

proc useTemp(c: var ControlFlow; tmp: TempVar; info: PackedLineInfo) =
  c.dest.copyIntoSymUse pool.syms.getOrIncl("`cf." & $int(tmp)), info

proc declareBool(c: var ControlFlow; info: PackedLineInfo): TempVar =
  result = TempVar(c.nextVar)
  inc c.nextVar
  c.dest.addParLe VarS, info
  c.defineTemp result, info
  c.dest.addEmpty2 info # no export marker, no pragmas
  c.dest.addParPair(BoolT, info)
  c.dest.addDotToken() # no value
  c.dest.addParRi()

proc rollbackToStmtBegin(c: var ControlFlow): TokenBuf =
  result = createTokenBuf(40)
  for i in c.stmtBegin ..< c.dest.len:
    result.add c.dest[i]
  c.dest.shrink c.stmtBegin

proc trStandaloneAndOr(c: var ControlFlow; n: var Cursor; opc: ExprKind) =
  assert opc == AndX or opc == OrX
  # The control flow graph has no `and`/`or` operators and we might already be in deeply
  # nested expression based code here. The solution is to "repair" the AST:
  # `(call (add x (add y z)) (and a b)` is rewritten to
  # `(var :tmp (bool)) (asgn tmp a) (ite tmp L1 L2)
  # L1: (asgn tmp b)
  # L2:
  # (call (add x (add y z)) tmp)`.
  # For this we stored the beginning of the stmt in `c.stmtBegin`.
  let fullExpr = rollbackToStmtBegin c
  let info = n.info
  let temp = declareBool(c, info)
  var tjmp: seq[Label] = @[]
  var fjmp: seq[Label] = @[]
  trIte c, n, tjmp, fjmp
  for t in tjmp:
    c.patch t
  c.dest.copyIntoKind(AsgnS, info):
    c.useTemp temp, info
    c.dest.addParPair TrueX, info
  let lend = c.jmpForw(info)
  # patch the false jump targets:
  for f in fjmp:
    c.patch f
  c.dest.copyIntoKind(AsgnS, info):
    c.useTemp temp, info
    c.dest.addParPair FalseX, info
  c.patch lend

  for i in 0 ..< fullExpr.len:
    c.dest.add fullExpr[i]
  c.useTemp temp, info

proc trWhile(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n
  let thisBlock = BlockOrLoop(kind: IsLoop, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  let loopStart = c.genLabel()

  # Generate if with goto
  var tjmp: seq[Label] = @[]
  trIte c, n, tjmp, thisBlock.breakInstrs # transform condition

  # loop body is about to begin:
  for t in tjmp: c.patch t

  trStmt(c, n) # transform body
  for cont in thisBlock.contInstrs: c.patch cont
  c.jmpBack(loopStart, info)

  for f in thisBlock.breakInstrs: c.patch f
  skipParRi n
  c.currentBlock = c.currentBlock.parent

proc trBlock(c: var ControlFlow; n: var Cursor) =
  inc n
  let thisBlock = BlockOrLoop(kind: IsBlock, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  if n.kind == SymbolDef:
    thisBlock.sym = n.symId
    inc n
  elif n.kind == DotToken:
    inc n
  else:
    raiseAssert "invalid block statement"
  trStmt c, n
  for brk in thisBlock.breakInstrs: c.patch brk
  skipParRi n
  c.currentBlock = c.currentBlock.parent

proc trReturn(c: var ControlFlow; n: var Cursor) =
  var it {.cursor.} = c.currentBlock
  while it != nil and it.kind notin {IsRoutine, IsTryStmt, IsFinally}:
    it = it.parent
  if it == nil:
    raiseAssert "return outside of routine"
  inc n # skip `(ret`
  if (n.kind == Symbol and n.symId == it.sym) or (n.kind == DotToken):
    discard "do not generate `result = result`"
    inc n
  else:
    c.dest.addParLe(AsgnS, n.info)
    c.dest.addSymUse it.sym, n.info
    trExpr c, n
    c.dest.addParRi()
  skipParRi n
  it.breakInstrs.add c.jmpForw(n.info)

proc trIf(c: var ControlFlow; n: var Cursor) =
  var endings: seq[Label] = @[]
  inc n # if
  while true:
    let info = n.info
    let k = n.substructureKind
    if k == ElifS:
      inc n
      var tjmp: seq[Label] = @[]
      var fjmp: seq[Label] = @[]
      trIte c, n, tjmp, fjmp # condition
      for t in tjmp: c.patch t
      trStmt c, n # action
      endings.add c.jmpForw(info)
      for f in fjmp: c.patch f
      skipParRi n
    elif k == ElseS:
      inc n
      trStmt c, n
      skipParRi n
    else:
      break
  skipParRi n
  for i in countdown(endings.high, 0):
    c.patch(endings[i])

proc trBreak(c: var ControlFlow; n: var Cursor) =
  var it {.cursor.} = c.currentBlock
  inc n
  if n.kind == DotToken:
    while it != nil and it.kind notin {IsLoop, IsBlock}:
      if it.kind == IsRoutine:
        # we cannot cross routine boundaries!
        raiseAssert "break outside of loop"
      it = it.parent
    if it != nil:
      it.breakInstrs.add c.jmpForw(n.info)
    else:
      raiseAssert "break outside of loop"
    inc n
  elif n.kind == Symbol:
    let lab = n.symId
    while it != nil and it.sym != lab:
      if it.kind == IsRoutine:
        # we cannot cross routine boundaries!
        raiseAssert "could not find label"
      it = it.parent
    if it != nil:
      it.breakInstrs.add c.jmpForw(n.info)
    else:
      raiseAssert "could not find label"
    inc n
  else:
    raiseAssert "invalid break statement"
  skipParRi n

proc trContinue(c: var ControlFlow; n: var Cursor) =
  var it {.cursor.} = c.currentBlock
  inc n
  if n.kind == DotToken:
    if it != nil:
      it.contInstrs.add c.jmpForw(n.info)
    else:
      raiseAssert "continue outside of loop"
    inc n
  else:
    raiseAssert "invalid continue statement"
  skipParRi n

proc trFor(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  let loopStart = c.jmpForw(info)
  c.dest.addParLe(ForBindF, info)
  inc n
  # iterator call:
  trExpr c, n
  # bindings:
  takeTree c.dest, n
  c.dest.addParRi()
  # loop body:
  let thisBlock = BlockOrLoop(kind: IsLoop, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  trStmt c, n
  skipParRi n
  for cont in thisBlock.contInstrs: c.patch cont
  c.jmpBack(loopStart, info)
  for brk in thisBlock.breakInstrs: c.patch brk
  c.currentBlock = c.currentBlock.parent

proc trResult(c: var ControlFlow; n: var Cursor) =
  copyInto c.dest, n:
    if c.currentBlock.kind == IsRoutine:
      c.currentBlock.sym = n.symId
    takeLocalHeader c.typeCache, c.dest, n
    trExpr c, n

proc trLocal(c: var ControlFlow; n: var Cursor) =
  copyInto c.dest, n:
    takeLocalHeader c.typeCache, c.dest, n
    trExpr c, n

proc trProc(c: var ControlFlow; n: var Cursor) =
  let thisProc = BlockOrLoop(kind: IsRoutine, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisProc
  c.typeCache.openScope()
  copyInto c.dest, n:
    let isConcrete = takeRoutineHeader(c.typeCache, c.dest, n)
    if isConcrete:
      c.dest.addParLe(StmtsS, n.info)
      trStmt c, n
    else:
      takeTree c.dest, n
    for ret in thisProc.breakInstrs: c.patch ret
    if isConcrete:
      c.dest.addParRi() # StmtsS
  c.currentBlock = c.currentBlock.parent
  c.typeCache.closeScope()

proc trTry(c: var ControlFlow; n: var Cursor) =
  var thisBlock = BlockOrLoop(kind: IsTryStmt, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  inc n
  trStmt c, n
  let tryEnd = c.jmpForw(n.info)
  for ret in thisBlock.breakInstrs: c.patch ret
  thisBlock.breakInstrs.shrink 0

  var exceptEnds: seq[Label] = @[]
  while n.substructureKind == ExceptS:
    inc n
    takeTree c.dest, n # copy (except e as Type)
    trStmt c, n
    exceptEnds.add c.jmpForw(n.info)
    skipParRi n

  for exceptEnd in exceptEnds: c.patch exceptEnd
  c.patch tryEnd
  # Inside a `finally` `return` really means `return` again:
  c.currentBlock = c.currentBlock.parent

  if n.substructureKind == FinallyS:
    inc n
    trStmt c, n
    skipParRi n

  skipParRi n

proc trRaise(c: var ControlFlow; n: var Cursor) =
  c.dest.addParLe(AsgnS, n.info)
  inc n
  c.dest.addSymUse pool.syms.getOrIncl("currexc.0.sys"), n.info
  trExpr c, n
  c.dest.addParRi()
  skipParRi n
  var it {.cursor.} = c.currentBlock
  while it != nil and it.kind notin {IsRoutine, IsTryStmt, IsFinally}:
    it = it.parent
  if it == nil:
    raiseAssert "raise outside of routine"
  else:
    it.breakInstrs.add c.jmpForw(n.info)

proc trAsgn(c: var ControlFlow; n: var Cursor) =
  copyInto c.dest, n:
    trExpr c, n
    trExpr c, n

proc trCaseSet(c: var ControlFlow; n: var Cursor; selector: SymId; selectorType: Cursor;
               tjmp, fjmp: var FixupList) =
  assert n.exprKind == SetX
  inc n
  var nextAttempt = Label(-1)
  while n.kind != ParRi:
    if nextAttempt.int >= 0:
      c.patch nextAttempt
    if n.exprKind == RangeX:
      raiseAssert "not implemented"
    else:
      c.dest.addParLe(IteF, n.info)
      c.dest.addParLe(EqX, n.info)
      c.dest.copyTree selectorType
      c.dest.addSymUse selector, n.info
      trExpr c, n
      c.dest.addParRi() # EqX
      tjmp.add c.jmpForw(n.info)
      nextAttempt = c.jmpForw(n.info)
      c.dest.addParRi() # IteF
  if nextAttempt.int >= 0:
    fjmp.add nextAttempt
  inc n

proc trCase(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n
  let selectorType = c.typeCache.getType(n)
  let simpleSelector = n.kind == Symbol
  var selector: SymId
  if simpleSelector:
    selector = n.symId
    inc n
  else:
    selector = pool.syms.getOrIncl("`cf" & $c.nextVar)
    inc c.nextVar
    c.dest.addParLe VarS, info
    c.dest.addSymDef selector, info
    c.dest.addEmpty2 info # no export marker, no pragmas
    c.dest.copyTree selectorType
    trExpr c, n
    c.dest.addParRi()

  var endings: FixupList = @[]
  while n.substructureKind == OfS:
    inc n
    var tjmp: FixupList = @[]
    var fjmp: FixupList = @[]
    trCaseSet c, n, selector, selectorType, tjmp, fjmp
    for t in tjmp: c.patch t
    trStmt c, n
    endings.add c.jmpForw(n.info)
    for f in fjmp: c.patch f
    skipParRi n
  if n.substructureKind == ElseS:
    inc n
    trStmt c, n
    skipParRi n
  skipParRi n
  for e in endings: c.patch e

proc trStmt(c: var ControlFlow; n: var Cursor) =
  c.stmtBegin = c.dest.len
  case n.stmtKind
  of NoStmt:
    trExpr c, n
  of IfS:
    trIf(c, n)
  of WhileS:
    trWhile(c, n)
  of StmtsS:
    inc n
    while n.kind != ParRi:
      trStmt c, n
    inc n
  of ScopeS:
    c.dest.add n
    inc n
    c.typeCache.openScope()
    while n.kind != ParRi:
      trStmt c, n
    c.dest.addParRi()
    inc n
    c.typeCache.closeScope()
  of BreakS:
    trBreak c, n
  of ContinueS:
    trContinue c, n
  of RetS:
    trReturn c, n
  of ResultS:
    trResult c, n
  of VarS, LetS, CursorS, ConstS:
    trLocal c, n
  of BlockS:
    trBlock c, n
  of ForS:
    trFor c, n
  of AsgnS:
    trAsgn c, n
  of CaseS:
    trCase c, n
  of TryS:
    trTry c, n
  of RaiseS:
    trRaise c, n
  of IterS, ProcS, FuncS, MacroS, ConverterS, MethodS:
    trProc c, n
  of TemplateS, TypeS, CommentS, EmitS, IncludeS, ImportS, ExportS, FromImportS, ImportExceptS, PragmasLineS:
    takeTree c.dest, n
  of YieldS, DiscardS, CallS, CmdS, InclSetS, ExclSetS:
    c.dest.add n
    inc n
    while n.kind != ParRi:
      trExpr c, n
    c.dest.addParRi()
    inc n
  of WhenS:
    raiseAssert "`when` statement should have been eliminated"

proc openTempVar(c: var ControlFlow; typ: Cursor; info: PackedLineInfo): TempVar =
  result = TempVar(c.nextVar)
  inc c.nextVar
  c.dest.addParLe LetS, info
  c.defineTemp result, info
  c.dest.addEmpty2 info # no export marker, no pragmas
  c.dest.copyTree typ

proc trStmtListExpr(c: var ControlFlow; n: var Cursor) =
  let typ = c.typeCache.getType(n)
  let info = n.info
  inc n
  let fullExpr = rollbackToStmtBegin c
  while n.kind != ParRi:
    if isLastSon(n): break
    trStmt c, n

  let temp = openTempVar(c, typ, info)
  trExpr c, n
  c.dest.addParRi() # close temp var declaration
  skipParRi n
  for i in 0 ..< fullExpr.len:
    c.dest.add fullExpr[i]
  c.useTemp temp, info

proc trExpr(c: var ControlFlow; n: var Cursor) =
  case n.kind
  of Symbol, SymbolDef, IntLit, UIntLit, FloatLit, StringLit, CharLit,
     Ident, DotToken, EofToken, UnknownToken:
    c.dest.add n
    inc n
  of ParRi:
    raiseAssert "unreachable"
  of ParLe:
    case n.exprKind
    of AndX:
      trStandaloneAndOr(c, n, AndX)
    of OrX:
      trStandaloneAndOr(c, n, OrX)
    of ExprX:
      trStmtListExpr c, n
    else:
      c.dest.add n
      inc n
      while n.kind != ParRi:
        trExpr c, n
      c.dest.addParRi()
      inc n

proc toControlflow*(n: Cursor): TokenBuf =
  var c = ControlFlow(typeCache: createTypeCache())
  assert n.stmtKind == StmtsS
  c.typeCache.openScope()
  var n = n
  c.dest.add n
  inc n
  while n.kind != ParRi:
    trStmt c, n
  c.dest.addParRi()
  c.typeCache.closeScope()
  result = ensureMove c.dest

when isMainModule:
  proc test(s: string) =
    var input = parse(s)
    var cf = toControlflow(beginRead(input))
    echo codeListing(cf)

  const BasicTest = """(stmts
(if (elif (eq +11 +11) (call echo "true")))

(if
  (elif (eq +12 +12) (call echo "true"))
  (elif (and (eq +2 +3) (eq +4 +5)) (call echo "elif"))
  (else (call echo "false"))
)

(while (eq +13 +13) (call echo "while"))

(while (or (eq +9 +9) (eq +4 +5)) (call echo "while 2"))

(let :my.var . . (i -1) (call echo.0 "abc" (and (eq +5 -5) (eq +6 -6))))
)

"""
  const NotTest = """(stmts
  (if (elif (not (eq +1 +1)) (call echo "true")))
  (call echo (expr (stmts (call side.effect)) +3))
)
"""
  const ReturnTest = """(stmts
  (proc :my.proc . . . (params (param :i.0 .. (i -1) .))
    (i -1) . . (stmts (result :res.0 . . (i -1) .) (ret +1)))
  (call my.proc +3))
  """

  const TryTest =  """(stmts
  (try
    (stmts (call echo "try") (raise some.exc))
    (except (as :e.0 Type)
      (stmts (call echo "except")))
    (fin
      (stmts (call echo "finally"))
    )
  ))
  """

  const CaseTest = """(stmts
    (let :my.var . . (i -1) .)
    (case my.var
      (of (set +0 +1 +2) (call echo "match"))
      (else (call echo "no match"))
    )
  )"""

  #test BasicTest
  #test NotTest
  #test ReturnTest
  #test TryTest
  test CaseTest
