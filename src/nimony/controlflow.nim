#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Helper to translate control flow into `goto` based code
## which can be easier to analyze, depending on the used algorithm.

when defined(nimony):
  {.feature: "lenientnils".}

import std/[assertions, intsets]
include ".." / lib / nifprelude

import ".." / models / tags
import nimony_model, programs, builtintypes, typenav, decls
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
    src: seq[int32]
      ## Side-channel parallel to `t`: `src[i]` is the source-buffer position of
      ## the source token that produced `t[i]`, or -1 for a synthesized token.
      ## Maintained lazily — trailing synthesized tokens are `-1`-padded on demand
      ## (see `padSrc`). Replaces the old scheme of smuggling the source position
      ## through the token's `info` field as a payload; nifcore tokens do not
      ## necessarily carry line info, so the mapping lives in a dedicated seq.
  ControlFlow = object
    dest: TokenBuf
    destSrc: seq[int32]         ## side-channel parallel to `dest`; see `Target.src`.
    srcBase: Cursor            ## start of the source buffer; source positions are
                               ## measured relative to it (`cursorToPosition`).
    nextVar: int
    currentBlock: BlockOrLoop
    typeCache: TypeCache
    resultSym: SymId
    keepReturns: bool

proc codeListing*(c: TokenBuf, start = 0; last = -1): string =
  result = "<codeListing: not available under nifcore>"
  # ── source-position side-channel ──────────────────────────────────────────
  # The mover needs to map every CF token back to the source token it came from.
  # Rather than stamp a payload into the token `info` field (nifcore tokens may
  # carry no line info), we thread a parallel `seq[int32]` of source positions
  # through the `Target`/`dest` buffers. Only actual source-token copies get a
  # real position (see `addSource`); everything synthesized is `-1`. The arrays
  # are kept in sync lazily: `padSrc` fills the gap with `-1` up to the buffer's
  # current length just before a precise append and once at the very end.

proc padSrcSeq(src: var seq[int32]; upTo: int) =
  while src.len < upTo: src.add(-1'i32)

proc padSrc(tar: var Target) = padSrcSeq(tar.src, tar.t.len)
proc pad(c: var ControlFlow) = padSrcSeq(c.destSrc, c.dest.len)

proc srcPosOf(c: ControlFlow; n: Cursor): int32 =
  int32(cursorToPosition(c.srcBase, n))

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
  var tok = c.dest[p.int]
  tok.patchInt28Token int32(diff)
  c.dest[p.int] = tok
proc trExpr(c: var ControlFlow; n: var Cursor; tar: var Target)
proc trStmt(c: var ControlFlow; n: var Cursor)

proc add(dest: var TokenBuf; tar: Target) =
  dest.copyTree tar.t

proc flush(c: var ControlFlow; tar: var Target) =
  ## Append a completed `Target` to `dest`, carrying its source-position
  ## side-channel along. Replaces the bare `c.dest.add tar`.
  padSrc(tar)
  pad(c)
  c.dest.add tar
  for s in tar.src: c.destSrc.add s

proc addSource(c: var ControlFlow; tar: var Target; n: Cursor) =
  ## Copy a single *source* token into `tar`, recording its source position.
  ## The only three call sites that funnel source tokens into the CF pipeline.
  padSrc(tar)
  if n.isTagLit:
    # register the head so the matching `addParRi` can seal it (a raw
    # token copy would leave a stale jump and nothing to close)
    tar.t.addParLe(n.tag, n.info)
  else:
    tar.t.add load(n)
  tar.src.add srcPosOf(c, n)

proc openTempVar(c: var ControlFlow; kind: StmtKind; typ: Cursor; info: PackedLineInfo): SymId =
  assert not typ.isDotToken
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
    src: seq[int32]

proc makeVar(c: var ControlFlow; info: PackedLineInfo; tar: var Target; typ: Cursor): TargetWrapper =
  case tar.m
  of IsVar:
    result = TargetWrapper(m: IsVar)
  of IsEmpty, IsIgnored, IsAppend:
    result = TargetWrapper(m: tar.m, t: move(tar.t), src: move(tar.src))
    let tmp = openTempVar(c, VarS, typ, info)
    c.dest.addDotToken()
    c.dest.addParRi()
    tar.m = IsVar
    tar.t = createTokenBuf(1)
    tar.src = @[]
    tar.t.addSymUse tmp, info

proc maybeAppend(tar: var Target; w: var TargetWrapper) =
  if w.m == IsAppend:
    padSrcSeq(w.src, w.t.len)
    padSrc(tar)
    w.t.add tar.t
    for s in tar.src: w.src.add s
    tar.t = move(w.t)
    tar.src = move(w.src)
  tar.m = w.m

proc trAndValue(c: var ControlFlow; n: var Cursor; tar: var Target) =
  # `tar = x and y` <=> `if x: tar = y else: tar = false`
  let info = n.info
  var w = makeVar(c, info, tar, c.typeCache.builtins.boolType)

  n.into:
    var aa = Target(m: IsEmpty)
    trExpr c, n, aa
    c.dest.addParLe(IteF, info)
    c.flush aa
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
      c.flush tar
      c.flush bb

    let lend = c.jmpForw(info)
    for f in fjmp: c.patch f
    assert tar.m == IsVar
    # tar = false:
    c.dest.copyIntoKind AsgnS, info:
      c.flush tar
      c.dest.addParPair(FalseX, info)
    c.patch lend
  maybeAppend tar, w

proc trOrValue(c: var ControlFlow; n: var Cursor; tar: var Target) =
  # `tar = x or y` <=> `if x: tar = true else: tar = y`
  let info = n.info
  var w = makeVar(c, info, tar, c.typeCache.builtins.boolType)

  n.into:
    var aa = Target(m: IsEmpty)
    trExpr c, n, aa
    c.dest.addParLe(IteF, info)
    c.flush aa
    var tjmp: seq[Label] = @[]
    var fjmp: seq[Label] = @[]
    tjmp.add c.jmpForw(info)
    fjmp.add c.jmpForw(info)
    c.dest.addParRi()
    for t in tjmp: c.patch t
    assert tar.m == IsVar
    # tar = true
    c.dest.copyIntoKind AsgnS, info:
      c.flush tar
      c.dest.addParPair(TrueX, info)
    let lend = c.jmpForw(info)
    for f in fjmp: c.patch f

    # tar = y
    assert tar.m == IsVar
    var bb = Target(m: IsEmpty)
    trExpr c, n, bb
    c.dest.copyIntoKind AsgnS, info:
      c.flush tar
      c.flush bb

    c.patch lend
  maybeAppend tar, w

proc trStmtListExpr(c: var ControlFlow; n: var Cursor; tar: var Target) =
  n.into:
    while n.hasMore:
      if not isLastSon(n):
        trStmt c, n
      else:
        trExpr c, n, tar

proc trExprLoop(c: var ControlFlow; n: var Cursor; tar: var Target) =
  if tar.m == IsEmpty:
    tar.m = IsAppend
  else:
    assert tar.m == IsAppend, toString(n, false) & " " & $tar.m
  c.addSource(tar, n)
  n.into:
    while n.hasMore:
      trExpr c, n, tar
  tar.t.addParRi()

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
    c.flush callTarget
    c.dest.addParRi()

    if tar.m == IsEmpty:
      tar = Target(m: IsVar)
    tar.t.addSymUse tmp, info
  else:
    trExprLoop c, n, tar

proc trVoidCall(c: var ControlFlow; n: var Cursor) =
  var tar = Target(m: IsAppend)
  c.addSource(tar, n)
  n.into:
    while n.hasMore:
      trExpr c, n, tar
  tar.t.addParRi()
  c.flush tar

proc trIte(c: var ControlFlow; n: var Cursor; tjmp, fjmp: var FixupList) =
  case n.exprKind
  of AndX:
    # `(x and y) goto (T, F)` <=>
    #     x goto (T1, F);
    # T1: y goto (T, F)
    n.into:
      var tjmpOverride: seq[Label] = @[]
      trIte c, n, tjmpOverride, fjmp
      for t in tjmpOverride: c.patch t
      trIte c, n, tjmp, fjmp
  of OrX:
    # `(x or y) goto (T, F)` <=>
    #     x goto (T, F1);
    # F1: y goto (T, F)
    n.into:
      var fjmpOverride: seq[Label] = @[]
      trIte c, n, tjmp, fjmpOverride
      for f in fjmpOverride: c.patch f
      trIte c, n, tjmp, fjmp
  of NotX:
    # reverse the jump targets:
    n.into:
      trIte c, n, fjmp, tjmp
  of ParX:
    n.into:
      trIte c, n, tjmp, fjmp
  of ErrX, SufX, AtX, DerefX, DotX, PatX, AddrX, NilX, InfX, NeginfX,
     NanX, FalseX, TrueX, XorX, NegX, SizeofX, AlignofX, OffsetofX,
     KvX, OconstrX, AconstrX, BracketX, CurlyX, CurlyatX, OvfX, AddX, SubX,
     MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX, BitnotX,
     EqX, NeqX, LeX, LtX, CastX, ConvX, CallX, CmdX, CchoiceX, OchoiceX,
     PragmaxX, QuotedX, HderefX, DdotX, HaddrX, NewrefX, NewobjX, TupX,
     TupconstrX, SetconstrX, TabconstrX, AshrX, BaseobjX, HconvX, DconvX,
     CallstrlitX, InfixX, PrefixX, HcallX, CompilesX, DeclaredX, DefinedX,
     AstToStrX, BindSymX, BindSymNameX, InstanceofX, ProccallX, HighX, LowX, TypeofX, UnpackX,
     FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, DefaultobjX,
     DefaulttupX, DefaultdistinctX, DelayX, Delay0X, SuspendX, ExprX,
     DoX, ArratX, TupatX, PlussetX, MinussetX, MulsetX, XorsetX, EqsetX,
     LesetX, LtsetX, InsetX, CardX, EmoveX, DestroyX, DupX, CopyX,
     WasmovedX, SinkhX, TraceX, InternalTypeNameX, InternalFieldPairsX,
     FailedX, IsX, EnvpX, ToClosureX, NoExpr:
    # cannot exploit a special case here:
    let info = NoLineInfo # NoLineInfo is crucial here!
    var bb = Target(m: IsEmpty)
    trExpr c, n, bb
    c.dest.addParLe(IteF, info)
    c.flush bb
    tjmp.add c.jmpForw(info)
    fjmp.add c.jmpForw(info)
    c.dest.addParRi()

proc trUseExpr(c: var ControlFlow; n: var Cursor) =
  var aa = Target(m: IsEmpty)
  trExpr c, n, aa
  c.flush aa

proc trStmtOrExpr(c: var ControlFlow; n: var Cursor; tar: var Target) =
  if tar.m != IsIgnored:
    var aa = Target(m: IsEmpty)
    # Capture the info up front: `trExpr` consumes `n`, so afterwards it may
    # sit on a (virtual) ParRi where `n.info` would assert under vpr.
    let info = n.info
    # it may be a `ExprX` so we generate statements before `AsgnS`
    trExpr c, n, aa
    c.dest.addParLe(AsgnS, info)
    assert tar.t.len > 0
    c.flush tar
    c.flush aa
    c.dest.addParRi()
  else:
    trStmt c, n

proc trIf(c: var ControlFlow; n: var Cursor; tar: var Target) =
  var endings: seq[Label] = @[]
  n.into: # if
    while n.hasMore:
      let info = n.info
      let k = n.substructureKind
      if k == ElifU:
        n.into:
          var tjmp: seq[Label] = @[]
          var fjmp: seq[Label] = @[]
          trIte c, n, tjmp, fjmp # condition
          for t in tjmp: c.patch t
          trStmtOrExpr c, n, tar # action
          endings.add c.jmpForw(info)
          for f in fjmp: c.patch f
      elif k == ElseU:
        n.into:
          trStmtOrExpr c, n, tar
          endings.add c.jmpForw(info) # this is crucial if we use the graph to compute basic blocks
      else:
        break
  for i in countdown(endings.high, 0):
    c.patch(endings[i])

proc trCaseRanges(c: var ControlFlow; n: var Cursor; selector: SymId; selectorType: Cursor;
               tjmp, fjmp: var FixupList) =
  assert n.substructureKind == RangesU
  var nextAttempt = Label(-1)
  var nextAttemptB = Label(-1)
  n.into:
    while n.hasMore:
      if nextAttempt.int >= 0:
        c.patch nextAttempt
        nextAttempt = Label(-1)
      if nextAttemptB.int >= 0:
        c.patch nextAttemptB
        nextAttemptB = Label(-1)

      if n.substructureKind == RangeU:
        n.into:
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
          tjmp.add c.jmpForw(n.endInfo)
          nextAttempt = c.jmpForw(n.endInfo)
          c.dest.addParRi() # IteF
      else:
        c.dest.addParLe(IteF, n.info)
        c.dest.addParLe(EqX, n.info)
        c.dest.copyTree selectorType
        c.dest.addSymUse selector, n.info
        trUseExpr c, n
        c.dest.addParRi() # EqX
        tjmp.add c.jmpForw(n.endInfo)
        nextAttempt = c.jmpForw(n.endInfo)
        c.dest.addParRi() # IteF
  if nextAttempt.int >= 0:
    fjmp.add nextAttempt
  if nextAttemptB.int >= 0:
    fjmp.add nextAttemptB

proc trCase(c: var ControlFlow; n: var Cursor; tar: var Target) =
  let info = n.info
  var endings: FixupList = @[]
  n.into:
    let selectorType = c.typeCache.getType(n)
    let isExhaustive = isOrdinalType(selectorType, allowEnumWithHoles=true)
    let simpleSelector = n.isSymbol
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
      c.flush aa
      c.dest.addParRi()

    var finalBranch = default(Cursor)
    if isExhaustive:
      var nn = n
      while nn.hasMore and nn.substructureKind == OfU:
        finalBranch = nn
        skip nn
      if nn.hasMore and nn.substructureKind == ElseU:
        finalBranch = default(Cursor)
    while n.hasMore and n.substructureKind == OfU:
      let isFinal = n == finalBranch
      n.into:
        if isFinal:
          # compile the final branch like an `else` to model the exhaustiveness precisely
          # in the control flow graph:
          skip n, SkipValue # ranges
          trStmtOrExpr c, n, tar
          endings.add c.jmpForw(n.endInfo) # this is crucial if we use the graph to compute basic blocks
        else:
          var tjmp: FixupList = @[]
          var fjmp: FixupList = @[]
          trCaseRanges c, n, selector, selectorType, tjmp, fjmp
          for t in tjmp: c.patch t
          trStmtOrExpr c, n, tar
          endings.add c.jmpForw(n.endInfo)
          for f in fjmp: c.patch f
    if n.hasMore and n.substructureKind == ElseU:
      n.into:
        trStmtOrExpr c, n, tar
        endings.add c.jmpForw(n.endInfo) # this is crucial if we use the graph to compute basic blocks
  for e in endings: c.patch e

proc trTry(c: var ControlFlow; n: var Cursor; tar: var Target) =
  var thisBlock = BlockOrLoop(kind: IsTryStmt, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  n.into:
    trStmtOrExpr c, n, tar
    let tryEnd = c.jmpForw(n.endInfo)
    for ret in thisBlock.breakInstrs: c.patch ret
    thisBlock.breakInstrs.shrink 0

    var exceptEnds: seq[Label] = @[]
    while n.hasMore and n.substructureKind == ExceptU:
      n.into:
        takeTree c.dest, n # copy (except e as Type)
        trStmtOrExpr c, n, tar
        exceptEnds.add c.jmpForw(n.endInfo)

    # `return` inside an `except` also collects into `thisBlock.breakInstrs`
    # (trReturn targets the innermost `IsTryStmt`); patch those here so they
    # land past all of the except bodies instead of as self-loop gotos.
    for ret in thisBlock.breakInstrs: c.patch ret
    thisBlock.breakInstrs.shrink 0

    for exceptEnd in exceptEnds: c.patch exceptEnd
    c.patch tryEnd
    # Inside a `finally` `return` really means `return` again:
    c.currentBlock = c.currentBlock.parent

    if n.hasMore and n.substructureKind == FinU:
      n.into:
        trStmt c, n

proc trBlock(c: var ControlFlow; n: var Cursor; tar: var Target) =
  let thisBlock = BlockOrLoop(kind: IsBlock, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  n.into:
    if n.isSymbolDef:
      thisBlock.sym = n.symId
      inc n
    elif n.isDotToken:
      inc n
    else:
      bug "invalid block statement"
    trStmtOrExpr c, n, tar
    for brk in thisBlock.breakInstrs: c.patch brk
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
  of Symbol, SymbolDef, IntLit, UIntLit, FloatLit, StrLitKind, CharLit,
     Ident, DotToken, EofTokenKind, UnknownTokenKind:
    c.addSource(tar, n)
    inc n
  of OpenTagKind:
    case n.exprKind
    of AndX:
      trAndValue c, n, tar
    of OrX:
      trOrValue c, n, tar
    of ExprX:
      trStmtListExpr c, n, tar
    of CallKinds:
      trCall c, n, tar
    of ArratX, TupatX, AtX, DerefX, HderefX, DotX, DdotX, PatX:
      # in anticipation of special casing:
      trExprLoop c, n, tar
    of AddrX, HaddrX:
      trExprLoop c, n, tar
    of QuotedX, ParX, CurlyatX, TabconstrX, DoX,
       NilX, FalseX, TrueX, NotX, NegX, KvX, OconstrX, NewobjX, NewrefX, TupconstrX,
       AconstrX, SetconstrX, OchoiceX, CchoiceX, AddX, SubX, MulX, DivX, ModX,
       ShrX, ShlX, AshrX, BitandX, BitorX, BitxorX, BitnotX, EqX, NeqX, LeX, LtX,
       CastX, ConvX, BaseobjX, HconvX, DconvX, InfX, NeginfX, NanX, SufX,
       UnpackX, FieldsX, FieldpairsX, EnumtostrX, XorX,
       IsmainmoduleX, DefaultobjX, DefaulttupX, DefaultdistinctX, PlussetX, MinussetX,
       MulsetX, XorsetX, EqsetX, LesetX, LtsetX, InsetX, CardX, EmoveX,
       DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX,
       BracketX, CurlyX, TupX, OvfX, InstanceofX, InternalFieldPairsX,
       FailedX, IsX, EnvpX, Delay0X, SuspendX, ToClosureX:
      trExprLoop c, n, tar
    of PragmaxX:
      bug "pragmax should be handled in trStmt"
    of CompilesX, DeclaredX, DefinedX, AstToStrX, BindSymX, BindSymNameX, HighX, LowX, TypeofX, SizeofX, AlignofX, OffsetofX, InternalTypeNameX:
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
      of CallS, CmdS, GvarS, TvarS, VarS, ConstS, ResultS, GletS, TletS,
         LetS, CursorS, PatternvarS, ProcS, FuncS, IteratorS, ConverterS,
         MethodS, MacroS, TemplateS, TypeS, EmitS, AsgnS, ScopeS, WhenS,
         BreakS, ContinueS, ForS, WhileS, CoroforS, RetS, YldS, StmtsS,
         PragmasS, PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
         FromimportS, ImportexceptS, ExportS, ExportexceptS, CommentS,
         DiscardS, RaiseS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
         InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
         AsmS, DeferS, NoStmt:
        trExprLoop c, n, tar
  else:
    bug "unreachable"

proc trWhile(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  let thisBlock = BlockOrLoop(kind: IsLoop, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  let loopStart = c.genLabel()

  n.into:
    # Generate if with goto
    var tjmp: seq[Label] = @[]
    trIte c, n, tjmp, thisBlock.breakInstrs # transform condition

    # loop body is about to begin:
    for t in tjmp: c.patch t

    trStmt(c, n) # transform body
    for cont in thisBlock.contInstrs: c.patch cont
    c.jmpBack(loopStart, info)

    for f in thisBlock.breakInstrs: c.patch f
  c.currentBlock = c.currentBlock.parent

proc trCoroFor(c: var ControlFlow; n: var Cursor) =
  let info = n.info
  let thisBlock = BlockOrLoop(kind: IsLoop, sym: SymId(0), parent: c.currentBlock)
  c.currentBlock = thisBlock
  let loopStart = c.genLabel()

  n.into:
    # First child is the iter call. cps.nim will rewrite this into the init+advance
    # trampoline; for CFG purposes treat it as a side-effecting expression whose
    # result is discarded.
    var aa = Target(m: IsAppend)
    trExpr c, n, aa
    if aa.t.len > 0:
      c.flush aa

    trStmt(c, n) # body
    for cont in thisBlock.contInstrs: c.patch cont
    c.jmpBack(loopStart, info)

    for f in thisBlock.breakInstrs: c.patch f
  c.currentBlock = c.currentBlock.parent

proc trReturn(c: var ControlFlow; n: var Cursor) =
  let orig = n
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
  n.into: # skip `(ret`
    if c.keepReturns:
      var aa = Target(m: IsEmpty)
      trExpr c, n, aa
      c.dest.addParLe(RetS, info)
      c.flush aa
      c.dest.addParRi()
    elif (n.isSymbol and n.symId == c.resultSym) or (n.isDotToken):
      discard "do not generate `result = result`"
      inc n
    else:
      if c.resultSym == NoSymId:
        bug "result symbol not found " & toString(orig, false)
      var aa = Target(m: IsEmpty)
      trExpr c, n, aa
      c.dest.addParLe(AsgnS, n.endInfo)
      c.dest.addSymUse c.resultSym, n.endInfo
      c.flush aa
      c.dest.addParRi()
  control.breakInstrs.add c.jmpForw(n.endInfo)

proc trBreak(c: var ControlFlow; n: var Cursor) =
  var it {.cursor.} = c.currentBlock
  n.into:
    if n.isDotToken:
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
    elif n.isSymbol:
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

proc trContinue(c: var ControlFlow; n: var Cursor) =
  var it {.cursor.} = c.currentBlock
  n.into:
    if n.isDotToken:
      if it != nil:
        it.contInstrs.add c.jmpForw(n.info)
      else:
        bug "continue outside of loop"
      inc n
    else:
      bug "invalid continue statement"

proc trFor(c: var ControlFlow; n: var Cursor) =
  let info = n.info
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
  n.into:
    c.dest.addParLe(ForbindF, info)
    # iterator call:
    trUseExpr c, n
    # bindings:
    takeTree c.dest, n
    c.dest.addParRi()
    # loop body:
    trStmt c, n
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
  skip n, SkipName # name
  skip n, SkipExport # export marker
  skip n, SkipPragmas # pragmas
  #c.typeCache.registerLocal(name, kind, n)
  skip n, SkipType # type

  var aa = Target(m: IsEmpty)
  trExpr c, n, aa
  n = orig
  copyInto c.dest, n:
    takeLocalHeader c.typeCache, c.dest, n, kind
    skip n, SkipValue # value
    c.flush aa

proc trRaise(c: var ControlFlow; n: var Cursor) =
  # we map `raise x` to `localErr = x; return`.
  let info = n.info
  n.into:
    var aa = Target(m: IsEmpty)
    trExpr c, n, aa
    c.dest.addParLe(AsgnS, info)
    c.dest.addSymUse pool.syms.getOrIncl("localErr.0." & SystemModuleSuffix), info
    c.flush aa
    c.dest.addParRi()
  var it {.cursor.} = c.currentBlock
  while it != nil and it.kind notin {IsRoutine, IsTryStmt, IsFinally}:
    it = it.parent
  if it == nil:
    bug "raise outside of routine"
  else:
    it.breakInstrs.add c.jmpForw(info)

proc isComplexLhs(n: Cursor): bool =
  var n = n
  if n.isTagLit and n.exprKind in CallKinds+{PatX, ArratX}:
    return true
  n.linearScan:
    if n.exprKind in CallKinds+{PatX, ArratX}:
      return true
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
  let headTag = n.tag
  var typ = default(Cursor)

  n.into:
    typ = c.typeCache.getType(n)
    trExpr c, n, aa
    assert aa.t.len > 0
    trExpr c, n, bb
    assert bb.t.len > 0
  c.dest.addParLe(headTag, info)
  c.flush aa
  c.flush bb
  c.dest.addParRi()

  # the `(asgn` head may carry line-info suffix tokens
  let lhsPos = asgnBegin + tokenWidth(readonlyCursorAt(c.dest, asgnBegin))
  let lhs = cursorAt(c.dest, lhsPos)
  if isComplexLhs(lhs):
    # The lhs/rhs of the just-emitted `asgn` are copied around below (and `dest`
    # is truncated), so the source-position side-channel must be reshuffled with
    # them. Align `destSrc` to `dest` first, then splice the matching slices into
    # the rewritten `stmts` (via a parallel `stmtsSrc`).
    c.pad()
    var rhs = lhs
    skip rhs
    let rhsPos = cursorToPosition(c.dest, rhs)

    var stmts = createTokenBuf(40)
    var stmtsSrc: seq[int32] = @[]

    let tmp = pool.syms.getOrIncl("`cf." & $c.nextVar)
    inc c.nextVar
    stmts.addParLe LetS, info
    stmts.addSymDef tmp, info
    stmts.addEmpty2 info # no export marker, no pragmas
    stmts.copyIntoKind PtrT, info:
      stmts.copyTree typ
    stmts.copyIntoKind AddrX, info:
      padSrcSeq(stmtsSrc, stmts.len)
      let beforeL = stmts.len
      stmts.copyTree lhs
      var kL = 0
      while beforeL + kL < stmts.len:
        stmtsSrc.add c.destSrc[lhsPos + kL]
        inc kL
    stmts.addParRi()

    stmts.copyIntoKind AsgnS, info:
      stmts.copyIntoKind DerefX, info:
        stmts.addSymUse tmp, info
      padSrcSeq(stmtsSrc, stmts.len)
      let beforeR = stmts.len
      stmts.copyTree rhs
      var kR = 0
      while beforeR + kR < stmts.len:
        stmtsSrc.add c.destSrc[rhsPos + kR]
        inc kR
    padSrcSeq(stmtsSrc, stmts.len)

    endRead c.dest
    c.dest.shrink asgnBegin
    c.destSrc.setLen asgnBegin
    c.dest.add stmts
    for s in stmtsSrc: c.destSrc.add s
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
    var aa = Target(m: IsAppend)
    trExpr c, n, aa
    if aa.t.len > 0:
      c.flush aa
  of IfS:
    var aa = Target(m: IsIgnored)
    trIf c, n, aa
  of WhileS:
    trWhile c, n
  of StmtsS, UnpackdeclS:
    n.into:
      while n.hasMore:
        trStmt c, n
  of ScopeS, StaticstmtS:
    c.dest.addParLe(n.tag, n.info)
    c.typeCache.openScope()
    n.into:
      while n.hasMore:
        trStmt c, n
    c.dest.addParRi()
    c.typeCache.closeScope()
  of BreakS:
    trBreak c, n
  of ContinueS:
    trContinue c, n
  of RetS:
    trReturn c, n
  of ResultS:
    trResult c, n
  of VarS, LetS, CursorS, PatternvarS, ConstS, GvarS, TvarS, GletS, TletS:
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
  of TemplateS, TypeS, CommentS, EmitS, IncludeS, ImportS, ExportS, FromimportS, ImportexceptS, PragmasS,
     ImportasS, ExportexceptS, BindS, MixinS, UsingS:
    c.dest.addDotToken()
    skip n
  of CallKindsS, InclS, ExclS, AssumeS, AssertS:
    trVoidCall c, n
  of YldS, DiscardS, AsmS, DeferS:
    var tar = Target(m: IsAppend)
    let headTag = n.tag
    let headInfo = n.info
    n.into:
      while n.hasMore:
        trExpr c, n, tar
    c.dest.addParLe(headTag, headInfo)
    c.flush tar
    c.dest.addParRi()
  of PragmaxS:
    n.into:
      skip n # ignore pragmas
      trStmt c, n
  of WhenS:
    bug "`when` statement should have been eliminated"
  of CoroforS:
    trCoroFor c, n

proc toControlflowImpl(n: Cursor; keepReturns: bool; srcMap: var seq[int32]): TokenBuf =
  var c = ControlFlow(typeCache: createTypeCache(), keepReturns: keepReturns)
  c.srcBase = n
  c.typeCache.openScope()
  let sk = n.stmtKind
  var n = n
  if sk in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS}:
    trProc c, n
  else:
    assert sk == StmtsS
    c.dest.addParLe(n.tag, n.info)
    n.into:
      while n.hasMore:
        trStmt c, n
    addRet c
    c.dest.addParRi()
  c.typeCache.closeScope()
  c.pad()                       # fill trailing synthesized tokens with -1
  srcMap = ensureMove c.destSrc
  result = ensureMove c.dest
  #echo "result: ", codeListing(result)

proc toControlflow*(n: Cursor; keepReturns = false): TokenBuf =
  ## Build the goto-based control-flow representation. The source-position
  ## side-channel is discarded — for callers (e.g. contracts) that only need
  ## the CF graph, not the back-mapping to the original trees.
  var srcMap: seq[int32] = @[]
  result = toControlflowImpl(n, keepReturns, srcMap)

proc toControlflowWithMap*(n: Cursor; srcMap: var seq[int32];
                           keepReturns = false): TokenBuf =
  ## As `toControlflow`, but also returns `srcMap`, a seq parallel to the result
  ## buffer: `srcMap[i]` is the position (relative to `n`) of the source token
  ## that produced CF token `i`, or -1 for a synthesized token. The mover inverts
  ## this to locate an `emove` operand's landing site in the CF.
  result = toControlflowImpl(n, keepReturns, srcMap)

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

when isMainModule:
  import std / [syncio, os]
  proc main(infile, outputfile: string; keepReturns: bool) =
    var input = parseFromFile(infile)
    var cf = toControlflow(beginRead(input), keepReturns=keepReturns)
    try:
      writeFile(outputfile, codeListing(cf))
    except:
      quit "cannot write: " & outputfile

  main(paramStr(1), paramStr(2), paramCount() > 2)
