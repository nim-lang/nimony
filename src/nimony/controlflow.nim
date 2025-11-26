#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Helper to translate control flow into `goto` based code
## which can be easier to analyze, depending on the used algorithm.

import std/[assertions, intsets]
include ".." / lib / nifprelude

import ".." / models / tags
import nimony_model, programs, builtintypes, typenav
from typeprops import isOrdinalType

const
  GotoInstr* = InlineInt

type
  Label = distinct int
  BlockKind = enum
    IsRoutine
    IsLoop
    IsBlock
    IsTryStmt
    IsFinally
  BlockOrLoop {.acyclic.} = ref object
    kind: BlockKind
    sym: SymId # block label
    parent: BlockOrLoop
    breakInstrs: seq[Label]
    contInstrs: seq[Label]

  FixupList = seq[Label]

  Mode = enum
    IsEmpty, IsVar, IsAppend, IsIgnored
  Target = object
    m: Mode
    t: TokenBuf
  ControlFlow = object
    dest: TokenBuf
    nextVar: int
    currentBlock: BlockOrLoop
    typeCache: TypeCache
    resultSym: SymId
    keepReturns: bool

proc codeListing*(c: TokenBuf, start = 0; last = -1): string =
  # for debugging purposes
  # first iteration: compute all necessary labels:
  var jumpTargets = initIntSet()
  let last = if last < 0: c.len-1 else: min(last, c.len-1)
  for i in start..last:
    if c[i].kind == GotoInstr:
      jumpTargets.incl(i+c[i].getInt28)
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
      let diff = c[i].getInt28()
      if diff != 0:
        b.addIdent "L" & $(i+diff)
      else:
        b.addIdent "L<BUG HERE>" & $i
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
  let diff = p.int - c.dest.len
  assert diff < 0
  c.dest.add int28Token(diff.int32, info)

proc jmpForw(c: var ControlFlow; info: PackedLineInfo): Label =
  result = Label(c.dest.len)
  c.dest.add int28Token(0, info) # destination will be patched later

proc patch(c: var ControlFlow; p: Label) =
  # patch with current index
  let diff = c.dest.len - p.int
  assert diff != 0
  assert c.dest[p.int].kind == GotoInstr
  c.dest[p.int].patchInt28Token int32(diff)

proc trExpr(c: var ControlFlow; n: var Cursor; tar: var Target)
proc trStmt(c: var ControlFlow; n: var Cursor)

proc add(dest: var TokenBuf; tar: Target) =
  dest.copyTree tar.t

proc openTempVar(c: var ControlFlow; kind: StmtKind; typ: Cursor; info: PackedLineInfo): SymId =
  assert typ.kind != DotToken
  result = pool.syms.getOrIncl("`cf." & $c.nextVar)
  inc c.nextVar
  c.dest.addParLe kind, info
  c.dest.addSymDef result, info
  c.dest.addEmpty2 info # no export marker, no pragmas
  c.dest.copyTree typ

type
  TargetWrapper = object
    m: Mode
    t: TokenBuf

proc makeVar(c: var ControlFlow; info: PackedLineInfo; tar: var Target; typ: Cursor): TargetWrapper =
  case tar.m
  of IsVar:
    result = TargetWrapper(m: IsVar)
  of IsEmpty, IsIgnored, IsAppend:
    result = TargetWrapper(m: tar.m, t: move(tar.t))
    let tmp = openTempVar(c, VarS, typ, info)
    c.dest.addDotToken()
    c.dest.addParRi()
    tar.m = IsVar
    tar.t = createTokenBuf(1)
    tar.t.addSymUse tmp, info

proc maybeAppend(tar: var Target; w: var TargetWrapper) =
  if w.m == IsAppend:
    w.t.add tar.t
    tar.t = move(w.t)
  tar.m = w.m

proc trAndValue(c: var ControlFlow; n: var Cursor; tar: var Target) =
  # `tar = x and y` <=> `if x: tar = y else: tar = false`
  let info = n.info
  var w = makeVar(c, info, tar, c.typeCache.builtins.boolType)

  inc n
  var aa = Target(m: IsEmpty)
  trExpr c, n, aa
  c.dest.addParLe(IteF, info)
  c.dest.add aa
  var tjmp: seq[Label] = @[]
  var fjmp: seq[Label] = @[]
  tjmp.add c.jmpForw(info)
  fjmp.add c.jmpForw(info)
  c.dest.addParRi()
  for t in tjmp: c.patch t
  assert tar.m == IsVar
  # tar = y
  var bb = Target(m: IsEmpty)
  trExpr c, n, bb
  c.dest.copyIntoKind AsgnS, info:
    c.dest.add tar
    c.dest.add bb

  let lend = c.jmpForw(info)
  for f in fjmp: c.patch f
  assert tar.m == IsVar
  # tar = false:
  c.dest.copyIntoKind AsgnS, info:
    c.dest.add tar
    c.dest.addParPair(FalseX, info)
  c.patch lend
  skipParRi n
  maybeAppend tar, w

proc trOrValue(c: var ControlFlow; n: var Cursor; tar: var Target) =
  # `tar = x or y` <=> `if x: tar = true else: tar = y`
  let info = n.info
  var w = makeVar(c, info, tar, c.typeCache.builtins.boolType)

  inc n
  var aa = Target(m: IsEmpty)
  trExpr c, n, aa
  c.dest.addParLe(IteF, info)
  c.dest.add aa
  var tjmp: seq[Label] = @[]
  var fjmp: seq[Label] = @[]
  tjmp.add c.jmpForw(info)
  fjmp.add c.jmpForw(info)
  c.dest.addParRi()
  for t in tjmp: c.patch t
  assert tar.m == IsVar
  # tar = true
  c.dest.copyIntoKind AsgnS, info:
    c.dest.add tar
    c.dest.addParPair(TrueX, info)
  let lend = c.jmpForw(info)
  for f in fjmp: c.patch f

  # tar = y
  assert tar.m == IsVar
  var bb = Target(m: IsEmpty)
  trExpr c, n, bb
  c.dest.copyIntoKind AsgnS, info:
    c.dest.add tar
    c.dest.add bb

  c.patch lend
  skipParRi n
  maybeAppend tar, w

proc trStmtListExpr(c: var ControlFlow; n: var Cursor; tar: var Target) =
  inc n
  while n.kind != ParRi:
    if not isLastSon(n):
      trStmt c, n
    else:
      trExpr c, n, tar
  skipParRi n

proc trExprLoop(c: var ControlFlow; n: var Cursor; tar: var Target) =
  if tar.m == IsEmpty:
    tar.m = IsAppend
  else:
    assert tar.m == IsAppend, toString(n, false) & " " & $tar.m
  tar.t.add n
  inc n
  while n.kind != ParRi:
    trExpr c, n, tar
  tar.t.addParRi()
  inc n

proc trCall(c: var ControlFlow; n: var Cursor; tar: var Target) =
  if c.keepReturns and tar.m == IsAppend:
    # bind to a temporary variable:
    let tmp = pool.syms.getOrIncl("`cf." & $c.nextVar)
    inc c.nextVar
    let info = n.info
    c.dest.addParLe LetS, info
    c.dest.addSymDef tmp, info
    c.dest.addEmpty2 info # no export marker, no pragmas
    let typ = c.typeCache.getType(n)
    c.dest.copyTree typ

    var callTarget = Target(m: IsAppend)
    trExprLoop c, n, callTarget
    c.dest.add callTarget
    c.dest.addParRi()

    if tar.m == IsEmpty:
      tar = Target(m: IsVar)
    tar.t.addSymUse tmp, info
  else:
    trExprLoop c, n, tar

proc trVoidCall(c: var ControlFlow; n: var Cursor) =
  var tar = Target(m: IsAppend)
  tar.t.add n
  inc n
  while n.kind != ParRi:
    trExpr c, n, tar
  tar.t.addParRi()
  inc n
  c.dest.add tar

proc trIte(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  case n.exprKind
  of AndX:
    # `(x and y) goto (T, F)` <=>
    #     x goto (T1, F);
    # T1: y goto (T, F)
    inc n
    var tjmpOverride: seq[Label] = @[]
    trIte c, n, tjmpOverride, fjmp
    for t in tjmpOverride: c.patch t
    trIte c, n, tjmp, fjmp
    skipParRi n
  of OrX:
    # `(x or y) goto (T, F)` <=>
    #     x goto (T, F1);
    # F1: y goto (T, F)
    inc n
    var fjmpOverride: seq[Label] = @[]
    trIte c, n, tjmp, fjmpOverride
    for f in fjmpOverride: c.patch f
    trIte c, n, tjmp, fjmp
    skipParRi n
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
    let info = NoLineInfo # NoLineInfo is crucial here!
    var bb = Target(m: IsEmpty)
    trExpr c, n, bb
    c.dest.addParLe(IteF, info)
    c.dest.add bb
    tjmp.add c.jmpForw(info)
    fjmp.add c.jmpForw(info)
    c.dest.addParRi()

proc trUseExpr(c: var ControlFlow; n: var Cursor) =
  var aa = Target(m: IsEmpty)
  trExpr c, n, aa
  c.dest.add aa

proc trStmtOrExpr(c: var ControlFlow; n: var Cursor; tar: var Target) =
  if tar.m != IsIgnored:
    var aa = Target(m: IsEmpty)
    # it may be a `ExprX` so we generate statements before `AsgnS`
    trExpr c, n, aa
    c.dest.addParLe(AsgnS, n.info)
    assert tar.t.len > 0
    c.dest.add tar
    c.dest.add aa
    c.dest.addParRi()
  else:
    trStmt c, n

proc trIf(c: var ControlFlow; n: var Cursor; tar: var Target) =
  var endings: seq[Label] = @[]
  inc n # if
  while true:
    let info = n.info
    let k = n.substructureKind
    if k == ElifU:
      inc n
      var tjmp: seq[Label] = @[]
      var fjmp: seq[Label] = @[]
      trIte c, n, tjmp, fjmp # condition
      for t in tjmp: c.patch t
      trStmtOrExpr c, n, tar # action
      endings.add c.jmpForw(info)
      for f in fjmp: c.patch f
      skipParRi n
    elif k == ElseU:
      inc n
      trStmtOrExpr c, n, tar
      endings.add c.jmpForw(info) # this is crucial if we use the graph to compute basic blocks
      skipParRi n
    else:
      break
  skipParRi n
  for i in countdown(endings.high, 0):
    c.patch(endings[i])

proc trCaseRanges(c: var ControlFlow; n: var Cursor; selector: SymId; selectorType: Cursor;
               tjmp, fjmp: var FixupList) =
  assert n.substructureKind == RangesU
  inc n
  var nextAttempt = Label(-1)
  var nextAttemptB = Label(-1)
  while n.kind != ParRi:
    if nextAttempt.int >= 0:
      c.patch nextAttempt
      nextAttempt = Label(-1)
    if nextAttemptB.int >= 0:
      c.patch nextAttemptB
      nextAttemptB = Label(-1)

    if n.substructureKind == RangeU:
      inc n

      c.dest.addParLe(IteF, n.info)
      c.dest.addParLe(LeX, n.info)
      c.dest.copyTree selectorType
      trUseExpr c, n
      c.dest.addSymUse selector, n.info
      c.dest.addParRi() # LeX
      let trange = c.jmpForw(n.info)
      nextAttemptB = c.jmpForw(n.info)
      c.dest.addParRi() # IteF
      c.patch trange

      c.dest.addParLe(IteF, n.info)
      c.dest.addParLe(LeX, n.info)
      c.dest.copyTree selectorType
      c.dest.addSymUse selector, n.info
      trUseExpr c, n
      c.dest.addParRi() # LeX
      tjmp.add c.jmpForw(n.info)
      nextAttempt = c.jmpForw(n.info)
      c.dest.addParRi() # IteF

      skipParRi n
    else:
      c.dest.addParLe(IteF, n.info)
      c.dest.addParLe(EqX, n.info)
      c.dest.copyTree selectorType
      c.dest.addSymUse selector, n.info
      trUseExpr c, n
      c.dest.addParRi() # EqX
      tjmp.add c.jmpForw(n.info)
      nextAttempt = c.jmpForw(n.info)
      c.dest.addParRi() # IteF
  if nextAttempt.int >= 0:
    fjmp.add nextAttempt
  if nextAttemptB.int >= 0:
    fjmp.add nextAttemptB
  inc n

proc trCase(c: var ControlFlow; n: var Cursor; tar: var Target) =
  let info = n.info
  inc n
  let selectorType = c.typeCache.getType(n)
  let isExhaustive = isOrdinalType(selectorType, allowEnumWithHoles=true)
  let simpleSelector = n.kind == Symbol
  var selector: SymId
  if simpleSelector:
    selector = n.symId
    inc n
  else:
    var aa = Target(m: IsEmpty)
    trExpr c, n, aa

    selector = pool.syms.getOrIncl("`cf." & $c.nextVar)
    inc c.nextVar
    c.dest.addParLe VarS, info
    c.dest.addSymDef selector, info
    c.dest.addEmpty2 info # no export marker, no pragmas
    c.dest.copyTree selectorType
    c.dest.add aa
    c.dest.addParRi()

  var endings: FixupList = @[]
  var finalBranch = default(Cursor)
  if isExhaustive:
    var nn = n
    while nn.substructureKind == OfU:
      finalBranch = nn
      skip nn
    if nn.substructureKind == ElseU:
      finalBranch = default(Cursor)
  while n.substructureKind == OfU:
    if n == finalBranch:
      # compile the final branch like an `else` to model the exhaustiveness precisely
      # in the control flow graph:
      inc n
      skip n # ranges
      trStmtOrExpr c, n, tar
      endings.add c.jmpForw(n.info) # this is crucial if we use the graph to compute basic blocks
    else:
      inc n
      var tjmp: FixupList = @[]
      var fjmp: FixupList = @[]
      trCaseRanges c, n, selector, selectorType, tjmp, fjmp
      for t in tjmp: c.patch t
      trStmtOrExpr c, n, tar
      endings.add c.jmpForw(n.info)
      for f in fjmp: c.patch f
    skipParRi n
  if n.substructureKind == ElseU:
    inc n
    trStmtOrExpr c, n, tar
    endings.add c.jmpForw(n.info) # this is crucial if we use the graph to compute basic blocks
    skipParRi n
  skipParRi n
  for e in endings: c.patch e

proc trTry(c: var ControlFlow; n: var Cursor; tar: var Target) =
  var thisBlock = BlockOrLoop(kind: IsTryStmt, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  inc n
  trStmtOrExpr c, n, tar
  let tryEnd = c.jmpForw(n.info)
  for ret in thisBlock.breakInstrs: c.patch ret
  thisBlock.breakInstrs.shrink 0

  var exceptEnds: seq[Label] = @[]
  while n.substructureKind == ExceptU:
    inc n
    takeTree c.dest, n # copy (except e as Type)
    trStmtOrExpr c, n, tar
    exceptEnds.add c.jmpForw(n.info)
    skipParRi n

  for exceptEnd in exceptEnds: c.patch exceptEnd
  c.patch tryEnd
  # Inside a `finally` `return` really means `return` again:
  c.currentBlock = c.currentBlock.parent

  if n.substructureKind == FinU:
    inc n
    trStmt c, n
    skipParRi n

  skipParRi n

proc trBlock(c: var ControlFlow; n: var Cursor; tar: var Target) =
  inc n
  let thisBlock = BlockOrLoop(kind: IsBlock, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  if n.kind == SymbolDef:
    thisBlock.sym = n.symId
    inc n
  elif n.kind == DotToken:
    inc n
  else:
    bug "invalid block statement"
  trStmtOrExpr c, n, tar
  for brk in thisBlock.breakInstrs: c.patch brk
  skipParRi n
  c.currentBlock = c.currentBlock.parent

type
  ControlFlowAsExprKind = enum
    IfExpr, CaseExpr, TryExpr, BlockExpr

proc trIfCaseTryBlockExpr(c: var ControlFlow; n: var Cursor; kind: ControlFlowAsExprKind; tar: var Target) =
  if tar.m != IsAppend and tar.t.len == 1 and tar.t[0].kind == Symbol:
    # no need for yet another temporary
    case kind
    of IfExpr:
      trIf c, n, tar
    of CaseExpr:
      trCase c, n, tar
    of TryExpr:
      trTry c, n, tar
    of BlockExpr:
      trBlock c, n, tar
  else:
    # need a temporary:
    let info = n.info
    let temp = openTempVar(c, VarS, c.typeCache.getType(n), NoLineInfo)
    c.dest.addDotToken()
    c.dest.addParRi() # close temp var declaration
    var aa = Target(m: IsVar)
    aa.t.addSymUse temp, info
    case kind
    of IfExpr:
      trIf c, n, aa
    of CaseExpr:
      trCase c, n, aa
    of TryExpr:
      trTry c, n, aa
    of BlockExpr:
      trBlock c, n, aa
    tar.t.addSymUse temp, info

proc trExpr(c: var ControlFlow; n: var Cursor; tar: var Target) =
  case n.kind
  of Symbol, SymbolDef, IntLit, UIntLit, FloatLit, StringLit, CharLit,
     Ident, DotToken, EofToken, UnknownToken:
    tar.t.add n
    inc n
  of ParRi:
    bug "unreachable"
  of ParLe:
    case n.exprKind
    of AndX:
      trAndValue c, n, tar
    of OrX:
      trOrValue c, n, tar
    of ExprX:
      trStmtListExpr c, n, tar
    of CallKinds:
      trCall c, n, tar
    of ArrAtX, TupatX, AtX, DerefX, HderefX, DotX, DdotX, PatX:
      # in anticipation of special casing:
      trExprLoop c, n, tar
    of AddrX, HaddrX:
      trExprLoop c, n, tar
    of QuotedX, ParX, CurlyatX, TabconstrX, DoX,
       NilX, FalseX, TrueX, NotX, NegX, OconstrX, NewobjX, NewrefX, TupConstrX,
       AconstrX, SetConstrX, OchoiceX, CchoiceX, AddX, SubX, MulX, DivX, ModX,
       ShrX, ShlX, AshrX, BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX, LeX, LtX,
       CastX, ConvX, BaseobjX, HconvX, DconvX, InfX, NegInfX, NanX, SufX,
       UnpackX, FieldsX, FieldpairsX, EnumToStrX, XorX,
       IsMainModuleX, DefaultObjX, DefaultTupX, DefaultDistinctX, PlusSetX, MinusSetX,
       MulSetX, XorSetX, EqSetX, LeSetX, LtSetX, InSetX, CardX, EmoveX,
       DestroyX, DupX, CopyX, WasMovedX, SinkhX, TraceX,
       BracketX, CurlyX, TupX, OvfX, InstanceofX, ProccallX, InternalFieldPairsX,
       FailedX, IsX, EnvpX, DelayX:
      trExprLoop c, n, tar
    of PragmaxX:
      bug "pragmax should be handled in trStmt"
    of CompilesX, DeclaredX, DefinedX, AstToStrX, HighX, LowX, TypeofX, SizeofX, AlignofX, OffsetofX, InternalTypeNameX:
      # we want to avoid false dependencies for `sizeof(var)` as it doesn't really "use" the variable:
      tar.t.addDotToken()
      skip n
    of ErrX:
      trExprLoop c, n, tar
    of NoExpr:
      case n.stmtKind
      of IfS:
        trIfCaseTryBlockExpr c, n, IfExpr, tar
      of CaseS:
        trIfCaseTryBlockExpr c, n, CaseExpr, tar
      of TryS:
        trIfCaseTryBlockExpr c, n, TryExpr, tar
      of BlockS:
        trIfCaseTryBlockExpr c, n, BlockExpr, tar
      else:
        trExprLoop c, n, tar

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

proc trReturn(c: var ControlFlow; n: var Cursor) =
  var it {.cursor.} = c.currentBlock
  var control {.cursor.}: BlockOrLoop = nil
  while it != nil and it.kind != IsRoutine:
    if control == nil and it.kind in {IsTryStmt, IsFinally}:
      control = it
    it = it.parent

  if it == nil:
    bug "return outside of routine"
  if control == nil:
    control = it
  let info = n.info
  inc n # skip `(ret`
  if c.keepReturns:
    var aa = Target(m: IsEmpty)
    trExpr c, n, aa
    c.dest.addParLe(RetS, info)
    c.dest.add aa
    c.dest.addParRi()
  elif (n.kind == Symbol and n.symId == c.resultSym) or (n.kind == DotToken):
    discard "do not generate `result = result`"
    inc n
  else:
    var aa = Target(m: IsEmpty)
    trExpr c, n, aa
    c.dest.addParLe(AsgnS, n.info)
    c.dest.addSymUse c.resultSym, n.info
    c.dest.add aa
    c.dest.addParRi()
  skipParRi n
  control.breakInstrs.add c.jmpForw(n.info)

proc trBreak(c: var ControlFlow; n: var Cursor) =
  var it {.cursor.} = c.currentBlock
  inc n
  if n.kind == DotToken:
    while it != nil and it.kind notin {IsLoop, IsBlock}:
      if it.kind == IsRoutine:
        # we cannot cross routine boundaries!
        bug "break outside of loop"
      it = it.parent
    if it != nil:
      it.breakInstrs.add c.jmpForw(n.info)
    else:
      bug "break outside of loop"
    inc n
  elif n.kind == Symbol:
    let lab = n.symId
    while it != nil and it.sym != lab:
      if it.kind == IsRoutine:
        # we cannot cross routine boundaries!
        bug "could not find label"
      it = it.parent
    if it != nil:
      it.breakInstrs.add c.jmpForw(n.info)
    else:
      bug "could not find label"
    inc n
  else:
    bug "invalid break statement"
  skipParRi n

proc trContinue(c: var ControlFlow; n: var Cursor) =
  var it {.cursor.} = c.currentBlock
  inc n
  if n.kind == DotToken:
    if it != nil:
      it.contInstrs.add c.jmpForw(n.info)
    else:
      bug "continue outside of loop"
    inc n
  else:
    bug "invalid continue statement"
  skipParRi n

proc trFor(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  inc n
  let thisBlock = BlockOrLoop(kind: IsLoop, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  let loopStart = c.genLabel()

  c.dest.addParLe(IteF, info)
  c.dest.addParPair TrueX, info
  let tjmp = c.jmpForw(info)
  thisBlock.breakInstrs.add c.jmpForw(info)
  c.dest.addParRi()

  # loop body is about to begin:
  c.patch tjmp
  c.dest.addParLe(ForbindF, info)
  # iterator call:
  trUseExpr c, n
  # bindings:
  takeTree c.dest, n
  c.dest.addParRi()
  # loop body:
  trStmt c, n
  skipParRi n
  for cont in thisBlock.contInstrs: c.patch cont
  c.jmpBack(loopStart, info)
  for brk in thisBlock.breakInstrs: c.patch brk
  c.currentBlock = c.currentBlock.parent

proc trResult(c: var ControlFlow; n: var Cursor) =
  copyInto c.dest, n:
    c.resultSym = n.symId
    takeLocalHeader c.typeCache, c.dest, n, ResultY
    trUseExpr c, n

proc trLocal(c: var ControlFlow; n: var Cursor) =
  let kind = n.symKind
  let orig = n
  inc n
  skip n # name
  skip n # export marker
  skip n # pragmas
  #c.typeCache.registerLocal(name, kind, n)
  skip n # type

  var aa = Target(m: IsEmpty)
  trExpr c, n, aa
  n = orig
  copyInto c.dest, n:
    takeLocalHeader c.typeCache, c.dest, n, kind
    skip n # value
    c.dest.add aa

proc trRaise(c: var ControlFlow; n: var Cursor) =
  # we map `raise x` to `localErr = x; return`.
  let info = n.info
  inc n
  var aa = Target(m: IsEmpty)
  trExpr c, n, aa
  c.dest.addParLe(AsgnS, info)
  c.dest.addSymUse pool.syms.getOrIncl("localErr.0." & SystemModuleSuffix), info
  c.dest.add aa
  c.dest.addParRi()
  skipParRi n
  var it {.cursor.} = c.currentBlock
  while it != nil and it.kind notin {IsRoutine, IsTryStmt, IsFinally}:
    it = it.parent
  if it == nil:
    bug "raise outside of routine"
  else:
    it.breakInstrs.add c.jmpForw(info)

proc isComplexLhs(n: Cursor): bool =
  var n = n
  var nested = 0
  while true:
    case n.kind
    of ParLe:
      if n.exprKind in CallKinds+{PatX, ArrAtX}:
        return true
      inc nested
    of ParRi:
      dec nested
    else: discard
    if nested == 0: break
    inc n
  return false

proc trAsgn(c: var ControlFlow; n: var Cursor) =
  # Problem: Analysis of left-hand-side of assignments always is more complex
  # because of things like `obj.field[f(a, b, c)] = value` which contain simple
  # usages of `a, b, c`. Thus we break these into two statements:
  # obj.a.b.c.s[i] = value --> let tmp = addr(obj.a.b.c.s[i]); tmp[] = value
  # This works more reliably when we already eliminated the ExprX things, so we
  # do it afterwards:
  let asgnBegin = c.dest.len
  let info = n.info
  var aa = Target(m: IsEmpty)
  var bb = Target(m: IsEmpty)
  let head = n.load()
  inc n

  let typ = c.typeCache.getType(n)
  trExpr c, n, aa
  assert aa.t.len > 0
  trExpr c, n, bb
  assert bb.t.len > 0
  skipParRi n
  c.dest.add head
  c.dest.add aa
  c.dest.add bb
  c.dest.addParRi()

  let lhs = cursorAt(c.dest, asgnBegin+1)
  if isComplexLhs(lhs):
    var stmts = createTokenBuf(40)

    let tmp = pool.syms.getOrIncl("`cf." & $c.nextVar)
    inc c.nextVar
    stmts.addParLe LetS, info
    stmts.addSymDef tmp, info
    stmts.addEmpty2 info # no export marker, no pragmas
    stmts.copyIntoKind PtrT, info:
      stmts.copyTree typ
    stmts.copyIntoKind AddrX, info:
      stmts.copyTree lhs
    stmts.addParRi()

    var rhs = lhs
    skip rhs

    stmts.copyIntoKind AsgnS, info:
      stmts.copyIntoKind DerefX, info:
        stmts.addSymUse tmp, info
      stmts.copyTree rhs

    endRead c.dest
    c.dest.shrink asgnBegin
    c.dest.add stmts
  else:
    endRead c.dest

proc addRet(c: var ControlFlow) =
  c.dest.addParLe(RetS, NoLineInfo)
  if c.resultSym != SymId(0):
    c.dest.addSymUse c.resultSym, NoLineInfo
  else:
    c.dest.addDotToken()
  c.dest.addParRi()

proc trProc(c: var ControlFlow; n: var Cursor) =
  let decl = n
  let thisProc = BlockOrLoop(kind: IsRoutine, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisProc
  c.typeCache.openScope()
  copyInto c.dest, n:
    let isConcrete = takeRoutineHeader(c.typeCache, c.dest, decl, n)
    if isConcrete:
      c.dest.addParLe(StmtsS, n.info)
      trStmt c, n
      for ret in thisProc.breakInstrs: c.patch ret
      addRet c
    else:
      takeTree c.dest, n
      for ret in thisProc.breakInstrs: c.patch ret
    if isConcrete:
      c.dest.addParRi() # StmtsS
  c.currentBlock = c.currentBlock.parent
  c.typeCache.closeScope()

proc trStmt(c: var ControlFlow; n: var Cursor) =
  case n.stmtKind
  of NoStmt:
    if n.exprKind == PragmaxX:
      inc n
      skip n # ignore pragmas
      trStmt c, n
      skipParRi n
    else:
      var aa = Target(m: IsAppend)
      trExpr c, n, aa
      if aa.t.len > 0:
        c.dest.add aa
  of IfS:
    var aa = Target(m: IsIgnored)
    trIf c, n, aa
  of WhileS:
    trWhile c, n
  of StmtsS, UnpackDeclS:
    inc n
    while n.kind != ParRi:
      trStmt c, n
    inc n
  of ScopeS, StaticstmtS:
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
  of VarS, LetS, CursorS, ConstS, GvarS, TvarS, GletS, TletS:
    trLocal c, n
  of BlockS:
    var aa = Target(m: IsIgnored)
    trBlock c, n, aa
  of ForS:
    trFor c, n
  of AsgnS:
    trAsgn c, n
  of CaseS:
    var aa = Target(m: IsIgnored)
    trCase c, n, aa
  of TryS:
    var aa = Target(m: IsIgnored)
    trTry c, n, aa
  of RaiseS:
    trRaise c, n
  of IteratorS, ProcS, FuncS, MacroS, ConverterS, MethodS:
    trProc c, n
  of TemplateS, TypeS, CommentS, EmitS, IncludeS, ImportS, ExportS, FromimportS, ImportExceptS, PragmasS,
     ImportasS, ExportexceptS, BindS, MixinS, UsingS:
    c.dest.addDotToken()
    skip n
  of CallKindsS, InclS, ExclS, AssumeS, AssertS:
    trVoidCall c, n
  of YldS, DiscardS, AsmS, DeferS:
    var tar = Target(m: IsAppend)
    let head = n.load()
    inc n
    while n.kind != ParRi:
      trExpr c, n, tar
    inc n
    c.dest.add head
    c.dest.add tar
    c.dest.addParRi()
  of WhenS:
    bug "`when` statement should have been eliminated"

proc toControlflow*(n: Cursor; keepReturns = false): TokenBuf =
  var c = ControlFlow(typeCache: createTypeCache(), keepReturns: keepReturns)
  c.typeCache.openScope()
  let sk = n.stmtKind
  var n = n
  if sk in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS}:
    trProc c, n
  else:
    assert sk == StmtsS
    c.dest.add n
    inc n
    while n.kind != ParRi:
      trStmt c, n
    addRet c
    c.dest.addParRi()
  c.typeCache.closeScope()
  result = ensureMove c.dest
  #echo "result: ", codeListing(result)

proc eliminateDeadInstructions*(c: TokenBuf; start = 0; last = -1): seq[bool] =
  # Create a sequence to track which instructions are reachable
  result = newSeq[bool]((if last < 0: c.len else: last + 1) - start)
  let last = if last < 0: c.len-1 else: min(last, c.len-1)

  # Initialize with the start position
  var worklist = @[start]
  var processed = initIntSet()

  # Process the worklist
  while worklist.len > 0:
    let pos = worklist.pop()
    if pos > last or pos in processed:
      continue

    processed.incl(pos)
    result[pos - start] = true  # Mark as reachable

    # Handle different instruction types
    if c[pos].kind == GotoInstr:
      let diff = c[pos].getInt28
      if diff != 0:
        worklist.add(pos + diff)  # Add the target of the jump
        # For forward jumps, everything between the goto and its target is potentially unreachable
        if diff > 0:
          # Don't automatically continue to the next instruction after a goto
          continue
    elif cast[TagEnum](c[pos].tag) == IteTagId:
      # For if-then-else, process the condition and both branches
      var p = pos + 1
      # Skip the condition, marking it as reachable
      while p <= last and c[p].kind != GotoInstr:
        result[p - start] = true
        inc p

      if p <= last and c[p].kind == GotoInstr:
        # Process the then branch target
        let thenDiff = c[p].getInt28
        result[p - start] = true  # Mark the goto as reachable
        worklist.add(p + thenDiff)

        # Move to the else branch
        inc p
        if p <= last and c[p].kind == GotoInstr:
          # Process the else branch target
          let elseDiff = c[p].getInt28
          result[p - start] = true  # Mark the goto as reachable
          worklist.add(p + elseDiff)

          # Don't automatically continue to the next instruction after ITE
          continue

    # For regular instructions or after processing special instructions,
    # continue to the next instruction
    worklist.add(pos + 1)

const
  PayloadOffset* = 1'u32 # so that we don't use 0 as a payload

proc prepare*(buf: var TokenBuf): seq[PackedLineInfo] =
  result = newSeq[PackedLineInfo](buf.len)
  for i in 0..<buf.len:
    result[i] = buf[i].info
    buf[i].info = toPayload(i.uint32 + PayloadOffset)

proc restore*(buf: var TokenBuf; infos: seq[PackedLineInfo]) =
  for i in 0..<buf.len:
    buf[i].info = infos[i]

proc isMarked*(n: Cursor): bool {.inline.} =
  result = n.info == toPayload(0'u32)

proc doMark*(n: Cursor) {.inline.} =
  n.setInfo(toPayload(0'u32))

proc testOrSetMark*(n: Cursor): bool {.inline.} =
  if isMarked(n):
    result = true
  else:
    doMark(n)
    result = false

when isMainModule:
  import std / [syncio, os]
  proc main(infile, outputfile: string; keepReturns: bool) =
    var input = parseFromFile(infile)
    var cf = toControlflow(beginRead(input), keepReturns=keepReturns)
    writeFile(outputfile, codeListing(cf))

  main(paramStr(1), paramStr(2), paramCount() > 2)
