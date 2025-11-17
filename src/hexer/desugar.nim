# removes abstractions like set ops and ref object constructors

import std / [assertions]
include nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints,
  builtintypes, langmodes, renderer, reporters]
import hexer_context

type
  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    tempUseBufStack: seq[TokenBuf]
    activeChecks: set[CheckMode]
    pending: TokenBuf

proc declareTemp(c: var Context; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo): SymId =
  let s = "`desugar." & $c.counter & "." & c.thisModuleSuffix
  inc c.counter
  result = pool.syms.getOrIncl(s)
  dest.add tagToken("var", info)
  dest.addSymDef result, info
  dest.addDotToken() # export, pragmas
  dest.addDotToken()
  copyTree dest, typ # type

proc needsTemp(n: Cursor): bool =
  case n.kind
  of Symbol, IntLit, UIntLit, FloatLit, CharLit, StringLit:
    result = false
  of ParLe:
    var n = n
    case n.exprKind
    of NilX, FalseX, TrueX, InfX, NegInfX, NanX, SizeofX:
      result = false
    of ExprX:
      inc n
      let first = n
      skip n
      if n.kind == ParRi:
        # single element expr
        result = needsTemp(first)
      else:
        result = true
    of SufX:
      inc n
      result = needsTemp(n)
    of DconvX:
      inc n
      skip n
      result = needsTemp(n)
    of AtX, PatX, ArrAtX, TupatX, DotX, DdotX, ParX, AddrX, HaddrX:
      inc n
      while n.kind != ParRi:
        if needsTemp(n):
          return true
        skip n
      result = false
    else:
      result = true
  else:
    result = true

proc skipParRi(n: var Cursor) =
  if n.kind == ParRi:
    inc n
  else:
    bug "expected ')', but got: ", n

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor; isTopScope = false)

proc trSons(c: var Context; dest: var TokenBuf; n: var Cursor; isTopScope = false) =
  copyInto dest, n:
    while n.kind != ParRi:
      tr(c, dest, n, isTopScope)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc trProcBody(c: var Context; dest: var TokenBuf; n: var Cursor) =
  inc n # (stmts)
  while n.kind != ParRi:
    tr(c, dest, n)
  skipParRi n

proc trRoutineHeader(c: var Context; dest: var TokenBuf; decl: Cursor; n: var Cursor; pragmas: var Cursor): bool =
  # returns false if the routine is generic
  result = true # assume it is concrete
  let sym = n.symId
  for i in 0..<BodyPos:
    if i == ParamsPos:
      c.typeCache.registerParams(sym, decl, n)
    elif i == TypevarsPos:
      result = n.substructureKind != TypevarsU
    elif i == ProcPragmasPos:
      pragmas = n
    takeTree dest, n

proc trRequires(c: var Context; dest: var TokenBuf; pragmas: Cursor) =
  if not cursorIsNil(pragmas) and BoundCheck in c.activeChecks:
    let req = extractPragma(pragmas, RequiresP)
    if not cursorIsNil(req):
      let info = req.info
      dest.copyIntoKind IfS, info:
        dest.copyIntoKind ElifU, info:
          dest.copyIntoKind NotX, info:
            var n = req
            tr(c, dest, n)
          dest.copyIntoKind StmtsS, info:
            dest.copyIntoKind CallS, info:
              dest.addSymUse pool.syms.getOrIncl("panic.0." & SystemModuleSuffix), info
              let msg = infoToStr(pragmas.info) & ": " & asNimCode(req) & " [AssertionDefect]\n"
              dest.addStrLit msg, info

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  let decl = n
  copyInto dest, n:
    var pragmas = default(Cursor)
    let isConcrete = c.trRoutineHeader(dest, decl, n, pragmas)
    if isConcrete and n.stmtKind == StmtsS:
      dest.add n # (stmts)
      trRequires(c, dest, pragmas)
      trProcBody(c, dest, n)
      dest.addParRi()
    else:
      takeTree dest, n
  c.typeCache.closeScope()

proc addUIntType(buf: var TokenBuf; bits: int; info: PackedLineInfo) =
  buf.add tagToken("u", info)
  buf.addIntLit(bits, info)
  buf.addParRi()

proc addIntType(buf: var TokenBuf; bits: int; info: PackedLineInfo) =
  buf.add tagToken("i", info)
  buf.addIntLit(bits, info)
  buf.addParRi()

proc addSetType(buf: var TokenBuf; size: int; info: PackedLineInfo) =
  case size
  of 1, 2, 4, 8:
    buf.addUIntType(size * 8, info)
  else:
    buf.add tagToken("array", info)
    buf.addUIntType(8, info)
    buf.addIntLit(size, info)
    buf.addParRi()

proc trSetType(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  let sizeOrig = bitsetSizeInBytes(n)
  var err = false
  let size = asSigned(sizeOrig, err)
  if err:
    error "invalid set element type: ", n
  else:
    addSetType dest, int size, info
  skip n
  skipParRi n

proc liftTemp(c: var Context; dest: var TokenBuf; n: Cursor; typ: Cursor; info: PackedLineInfo): Cursor =
  let tmp = declareTemp(c, dest, typ, n.info)
  dest.addSubtree n
  dest.addParRi()
  c.tempUseBufStack.add createTokenBuf(4)
  c.tempUseBufStack[^1].add symToken(tmp, n.info)
  result = beginRead(c.tempUseBufStack[^1])

proc liftTempAddr(c: var Context; dest: var TokenBuf; n: Cursor; typ: Cursor; info: PackedLineInfo): Cursor =
  var ptrTypeBuf = createTokenBuf(8)
  copyIntoKind ptrTypeBuf, PtrT, typ.info:
    ptrTypeBuf.addSubtree typ
  let ptrType = beginRead(ptrTypeBuf)
  let tmp = declareTemp(c, dest, ptrType, n.info)
  copyIntoKind dest, AddrX, n.info:
    dest.addSubtree n
  dest.addParRi()
  c.tempUseBufStack.add createTokenBuf(4)
  copyIntoKind c.tempUseBufStack[^1], DerefX, n.info:
    c.tempUseBufStack[^1].add symToken(tmp, n.info)
  result = beginRead(c.tempUseBufStack[^1])

template addTypedOp(dest: var TokenBuf; kind: ExprKind|StmtKind; typ: Cursor; info: PackedLineInfo; body: typed) =
  copyIntoKind dest, kind, info:
    dest.addSubtree typ
    body

template addUIntTypedOp(dest: var TokenBuf; kind: ExprKind|StmtKind; bits: int; info: PackedLineInfo; body: typed) =
  copyIntoKind dest, kind, info:
    dest.addUIntType(bits, info)
    body

template addIntTypedOp(dest: var TokenBuf; kind: ExprKind|StmtKind; bits: int; info: PackedLineInfo; body: typed) =
  copyIntoKind dest, kind, info:
    dest.addIntType(bits, info)
    body

template forRangeExclusive(c: var Context; dest: var TokenBuf; i: Cursor; bound: int; info: PackedLineInfo; body: typed) =
  copyIntoKind dest, WhileS, info:
    addIntTypedOp dest, LtX, -1, info:
      dest.addSubtree i
      dest.addIntLit(bound, info)
    copyIntoKind dest, StmtsS, info:
      body
      copyIntoKind dest, AsgnS, info:
        dest.addSubtree i
        addIntTypedOp dest, AddX, -1, info:
          dest.addSubtree i
          dest.addIntLit(1, info)

proc arrayToPointer(dest: var TokenBuf; arr: Cursor; info: PackedLineInfo) =
  copyIntoKind dest, AddrX, info:
    copyIntoKind dest, ArrAtX, info:
      dest.addSubtree arr
      dest.addIntLit(0, info)

proc genSetElem(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # XXX could implement offset here
  addUIntTypedOp dest, CastX, -1, n.info:
    tr(c, dest, n)

proc genSetOp(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let kind = n.exprKind
  inc n
  let typ = n
  if typ.typeKind != SetT:
    error "expected set type for set op", n
  var baseType = typ
  inc baseType
  var argsBuf = createTokenBuf(16)
  swap dest, argsBuf
  let typeStart = dest.len
  trSetType(c, dest, n)
  let aStart = dest.len
  tr(c, dest, n)
  let bStart = dest.len
  if kind == InSetX:
    genSetElem(c, dest, n)
  else:
    tr(c, dest, n)
  swap dest, argsBuf
  skipParRi n
  let cType = cursorAt(argsBuf, typeStart)
  let aOrig = cursorAt(argsBuf, aStart)
  let bOrig = cursorAt(argsBuf, bStart)
  let useTemp = needsTemp(aOrig) or needsTemp(bOrig)
  let oldBufStackLen = c.tempUseBufStack.len
  let a: Cursor
  let b: Cursor
  if useTemp:
    dest.add parLeToken(ExprX, info)
    # lift both so (n, (n = 123; n)) works
    a = liftTemp(c, dest, aOrig, typ, info)
    b = liftTemp(c, dest, bOrig, if kind == InSetX: c.typeCache.builtins.uintType else: typ, info)
  else:
    a = aOrig
    b = bOrig
  var err = false
  let size = int asSigned(bitsetSizeInBytes(baseType), err)
  assert not err
  case size
  of 1, 2, 4, 8:
    case kind
    of LtSetX:
      copyIntoKind dest, AndX, info:
        addTypedOp dest, EqX, cType, info:
          addTypedOp dest, BitAndX, cType, info:
            dest.addSubtree a
            addTypedOp dest, BitNotX, cType, info:
              dest.addSubtree b
          dest.addIntLit(0, info)
        addTypedOp dest, NeqX, cType, info:
          dest.addSubtree a
          dest.addSubtree b
    of LeSetX:
      addTypedOp dest, EqX, cType, info:
        addTypedOp dest, BitAndX, cType, info:
          dest.addSubtree a
          addTypedOp dest, BitNotX, cType, info:
            dest.addSubtree b
        dest.addIntLit(0, info)
    of EqSetX:
      addTypedOp dest, EqX, cType, info:
        dest.addSubtree a
        dest.addSubtree b
    of MulSetX:
      addTypedOp dest, BitAndX, cType, info:
        dest.addSubtree a
        dest.addSubtree b
    of PlusSetX:
      addTypedOp dest, BitOrX, cType, info:
        dest.addSubtree a
        dest.addSubtree b
    of MinusSetX:
      addTypedOp dest, BitAndX, cType, info:
        dest.addSubtree a
        addTypedOp dest, BitNotX, cType, info:
          dest.addSubtree b
    of XorSetX:
      addTypedOp dest, BitXorX, cType, info:
        dest.addSubtree a
        dest.addSubtree b
    of InSetX:
      let mask = size * 8 - 1
      addTypedOp dest, NeqX, cType, info:
        addTypedOp dest, BitAndX, cType, info:
          dest.addSubtree a
          addTypedOp dest, ShlX, cType, info:
            addTypedOp dest, CastX, cType, info:
              dest.addIntLit(1, info)
            addUIntTypedOp dest, BitAndX, -1, info:
              dest.addSubtree b
              dest.addUIntLit(uint64(mask), info)
        dest.addUIntLit(0, info)
    else:
      bug("unreachable")
  else:
    case kind
    of LtSetX, LeSetX:
      dest.add parLeToken(ExprX, info)
      let resValue = [parLeToken(TrueX, info), parRiToken(info)]
      let res = liftTemp(c, dest, fromBuffer(resValue), c.typeCache.builtins.boolType, info)
      let iValue = [intToken(pool.integers.getOrIncl(0), info)]
      let i = liftTemp(c, dest, fromBuffer(iValue), c.typeCache.builtins.intType, info)
      forRangeExclusive c, dest, i, size, info:
        copyIntoKind dest, AsgnS, info:
          dest.addSubtree res
          addUIntTypedOp dest, EqX, 8, info:
            addUIntTypedOp dest, BitandX, 8, info:
              copyIntoKind dest, ArrAtX, info:
                dest.addSubtree a
                dest.addSubtree i
              addUIntTypedOp dest, BitnotX, 8, info:
                copyIntoKind dest, ArrAtX, info:
                  dest.addSubtree b
                  dest.addSubtree i
            dest.addIntLit(0, info)
        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            copyIntoKind dest, NotX, info:
              dest.addSubtree res
            copyIntoKind dest, StmtsS, info:
              copyIntoKind dest, BreakS, info:
                dest.addDotToken()
      if kind == LtSetX:
        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            dest.addSubtree res
            copyIntoKind dest, StmtsS, info:
              copyIntoKind dest, AsgnS, info:
                dest.addSubtree res
                addIntTypedOp dest, NeqX, -1, info:
                  copyIntoKind dest, CallX, info:
                    dest.add symToken(pool.syms.getOrIncl("cmpMem.0." & SystemModuleSuffix), info)
                    dest.arrayToPointer(a, info)
                    dest.arrayToPointer(b, info)
                    dest.addIntLit(size, info)
                  dest.addIntLit(0, info)
      dest.addSubtree res
      dest.addParRi()
    of EqSetX:
      addIntTypedOp dest, EqX, -1, info:
        copyIntoKind dest, CallX, info:
          dest.add symToken(pool.syms.getOrIncl("cmpMem.0." & SystemModuleSuffix), info)
          dest.arrayToPointer(a, info)
          dest.arrayToPointer(b, info)
          dest.addIntLit(size, info)
        dest.addIntLit(0, info)
    of MulSetX, PlusSetX, MinusSetX, XorSetX:
      dest.add parLeToken(ExprX, info)
      let resValue = [dotToken(info)]
      let res = liftTemp(c, dest, fromBuffer(resValue), cType, info)
      let iValue = [intToken(pool.integers.getOrIncl(0), info)]
      let i = liftTemp(c, dest, fromBuffer(iValue), c.typeCache.builtins.intType, info)
      forRangeExclusive c, dest, i, size, info:
        copyIntoKind dest, AsgnS, info:
          copyIntoKind dest, ArrAtX, info:
            dest.addSubtree res
            dest.addSubtree i
          let op =
            case kind
            of PlusSetX: BitorX
            of XorSetX: BitxorX
            of MulSetX, MinusSetX: BitandX
            else: bug("unreachable")
          addUIntTypedOp dest, op, 8, info:
            copyIntoKind dest, ArrAtX, info:
              dest.addSubtree a
              dest.addSubtree i
            if op == MinusSetX:
              addUIntTypedOp dest, BitnotX, 8, info:
                copyIntoKind dest, ArrAtX, info:
                  dest.addSubtree b
                  dest.addSubtree i
            else:
              copyIntoKind dest, ArrAtX, info:
                dest.addSubtree b
                dest.addSubtree i
      dest.addSubtree res
      dest.addParRi()
    of InSetX:
      addUIntTypedOp dest, NeqX, 8, info:
        addUIntTypedOp dest, BitAndX, 8, info:
          copyIntoKind dest, ArrAtX, info:
            dest.addSubtree a
            addUIntTypedOp dest, ShrX, -1, info:
              dest.addSubtree b
              dest.addUintLit(3, info)
          addUIntTypedOp dest, ShlX, 8, info:
            dest.addUIntLit(1, info)
            addUIntTypedOp dest, BitAndX, -1, info:
              dest.addSubtree b
              dest.addUIntLit(7, info)
        dest.addUIntLit(0, info)
    else:
      bug("unreachable")
  if useTemp:
    dest.addParRi()
    c.tempUseBufStack.shrink(oldBufStackLen)

proc genCard(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  let typ = n
  if typ.typeKind != SetT:
    error "expected set type for set op", n
  var baseType = typ
  inc baseType
  var argsBuf = createTokenBuf(16)
  swap dest, argsBuf
  skip n # nothing to do with set type
  let aStart = dest.len
  tr(c, dest, n)
  swap dest, argsBuf
  skipParRi n
  let a = cursorAt(argsBuf, aStart) # no temp needed
  var err = false
  let size = asSigned(bitsetSizeInBytes(baseType), err)
  assert not err
  case size
  of 1, 2:
    copyIntoKind dest, CallX, info:
      dest.add symToken(pool.syms.getOrIncl("countBits32.0." & SystemModuleSuffix), info)
      addUIntTypedOp dest, CastX, 32, info:
        dest.addSubtree a
  of 4:
    copyIntoKind dest, CallX, info:
      dest.add symToken(pool.syms.getOrIncl("countBits32.0." & SystemModuleSuffix), info)
      dest.addSubtree a
  of 8:
    copyIntoKind dest, CallX, info:
      dest.add symToken(pool.syms.getOrIncl("countBits64.0." & SystemModuleSuffix), info)
      dest.addSubtree a
  else:
    copyIntoKind dest, CallX, info:
      dest.add symToken(pool.syms.getOrIncl("cardSet.0." & SystemModuleSuffix), info)
      dest.arrayToPointer(a, info)
      dest.addIntLit(size, info)

proc genSingleInclSmall(dest: var TokenBuf; s, elem: Cursor; size: int; info: PackedLineInfo) =
  let bits = size * 8
  copyIntoKind dest, AsgnS, info:
    dest.addSubtree s
    addUIntTypedOp dest, BitorX, bits, info:
      dest.addSubtree s
      addUIntTypedOp dest, ShlX, bits, info:
        addUIntTypedOp dest, CastX, bits, info:
          dest.addIntLit(1, info)
        addUIntTypedOp dest, ModX, bits, info:
          dest.addSubtree elem
          dest.addUIntLit(uint64(bits), info)

proc genSingleInclBig(dest: var TokenBuf; s, elem: Cursor; info: PackedLineInfo) =
  template addLhs() =
    copyIntoKind dest, ArrAtX, info:
      dest.addSubtree s
      addUIntTypedOp dest, ShrX, -1, info:
        addUIntTypedOp dest, CastX, -1, info:
          dest.addSubtree elem
        dest.addUIntLit(3, info)
  copyIntoKind dest, AsgnS, info:
    addLhs()
    addUIntTypedOp dest, BitorX, 8, info:
      addLhs()
      addUIntTypedOp dest, ShlX, 8, info:
        dest.addUIntLit(1, info)
        addUIntTypedOp dest, BitandX, -1, info:
          addUIntTypedOp dest, CastX, -1, info:
            dest.addSubtree elem
          dest.addUIntLit(7, info)

proc genSetConstrRuntime(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.add parLeToken(ExprX, info)
  inc n # tag
  let typ = n
  skip n
  var elemTyp = typ
  inc elemTyp
  var err = false
  let size = int asSigned(bitsetSizeInBytes(elemTyp), err)
  assert not err
  var typBuf = createTokenBuf(16)
  addSetType typBuf, size, info
  let cType = beginRead(typBuf)
  let big = size > 8
  let resValue =
    if big: [dotToken(info)]
    else: [uintToken(pool.uintegers.getOrIncl(0), info)]
  let res = liftTemp(c, dest, fromBuffer(resValue), cType, info)
  if big:
    copyIntoKind dest, CallX, info:
      dest.add symToken(pool.syms.getOrIncl("zeroMem.0." & SystemModuleSuffix), info)
      dest.arrayToPointer(res, info)
      dest.addIntLit(size, info)
  while n.kind != ParRi:
    let elemInfo = n.info
    if n.substructureKind == RangeU:
      inc n
      var argsBuf = createTokenBuf(16)
      swap dest, argsBuf
      let aStart = dest.len
      genSetElem(c, dest, n)
      let bStart = dest.len
      genSetElem(c, dest, n)
      swap dest, argsBuf
      skipParRi n
      # a is used once, no need for temp:
      let a = cursorAt(argsBuf, aStart)
      let bOrig = cursorAt(argsBuf, bStart)
      let useTemp = needsTemp(bOrig)
      let b: Cursor
      if useTemp:
        b = liftTemp(c, dest, bOrig, c.typeCache.builtins.uintType, elemInfo)
      else:
        b = bOrig
      let i = liftTemp(c, dest, a, c.typeCache.builtins.uintType, elemInfo)
      copyIntoKind dest, WhileS, elemInfo:
        addUIntTypedOp dest, LeX, -1, elemInfo:
          dest.addSubtree i
          dest.addSubtree b
        copyIntoKind dest, StmtsS, elemInfo:
          if big:
            genSingleInclBig(dest, res, i, elemInfo)
          else:
            genSingleInclSmall(dest, res, i, size, elemInfo)
          copyIntoKind dest, AsgnS, elemInfo:
            dest.addSubtree i
            addUIntTypedOp dest, AddX, -1, elemInfo:
              dest.addSubtree i
              dest.addUIntLit(1, elemInfo)
    else:
      var argsBuf = createTokenBuf(16)
      swap dest, argsBuf
      let aStart = dest.len
      genSetElem(c, dest, n)
      swap dest, argsBuf
      let aOrig = cursorAt(argsBuf, aStart)
      let useTemp = needsTemp(aOrig)
      let a: Cursor
      if useTemp:
        a = liftTemp(c, dest, aOrig, c.typeCache.builtins.uintType, elemInfo)
      else:
        a = aOrig
      if big:
        genSingleInclBig(dest, res, a, elemInfo)
      else:
        genSingleInclSmall(dest, res, a, size, elemInfo)
  skipParRi n
  dest.addSubtree res
  dest.addParRi()

proc genSetConstr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  var typ = c.typeCache.getType(n)
  var bytes = evalBitSet(n, typ)
  case bytes.len
  of 0:
    # not constant
    genSetConstrRuntime(c, dest, n)
  of 1, 2, 4, 8:
    # hopefully this is correct?
    bytes.setLen(8)
    dest.addUIntLit(cast[ptr uint64](addr bytes[0])[], info)
    skip n
  else:
    dest.addParLe(AconstrX, info)
    trSetType(c, dest, typ)
    for b in bytes:
      dest.addUIntLit(b, info)
    dest.addParRi()
    skip n

proc genInclExcl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let kind = n.stmtKind
  inc n
  let typ = n
  if typ.typeKind != SetT:
    error "expected set type for incl/excl", n
  var baseType = typ
  inc baseType
  var argsBuf = createTokenBuf(16)
  swap dest, argsBuf
  let typeStart = dest.len
  trSetType(c, dest, n)
  let aStart = dest.len
  tr(c, dest, n)
  let bStart = dest.len
  tr(c, dest, n)
  swap dest, argsBuf
  skipParRi n
  let cType = cursorAt(argsBuf, typeStart)
  let aOrig = cursorAt(argsBuf, aStart)
  let bOrig = cursorAt(argsBuf, bStart)
  let useTemp = needsTemp(aOrig) or needsTemp(bOrig)
  let oldBufStackLen = c.tempUseBufStack.len
  let a: Cursor
  let b: Cursor
  if useTemp:
    dest.add parLeToken(StmtsS, info)
    # lift both so (n, (n = 123; n)) works
    a = liftTempAddr(c, dest, aOrig, typ, info)
    b = liftTemp(c, dest, bOrig, typ.firstSon, info)
  else:
    a = aOrig
    b = bOrig
  var err = false
  let size = asSigned(bitsetSizeInBytes(baseType), err)
  assert not err
  case size
  of 1, 2, 4, 8:
    let mask = size * 8 - 1
    copyIntoKind dest, AsgnS, info:
      dest.addSubtree a
      if kind == InclS:
        dest.addParLe(BitOrX, info)
        dest.addSubtree cType
        dest.addSubtree a
      else:
        dest.addParLe(BitAndX, info)
        dest.addSubtree cType
        dest.addSubtree a
        dest.addParLe(BitNotX, info)
        dest.addSubtree cType
      addTypedOp dest, ShlX, cType, info:
        addTypedOp dest, CastX, cType, info:
          dest.addIntLit(1, info)
        addUIntTypedOp dest, BitAndX, -1, info:
          dest.addSubtree b
          dest.addIntLit(mask, info)
      if kind == InclS:
        dest.addParRi() # bitor
      else:
        dest.addParRi() # bitand
        dest.addParRi() # bitnot
  else:
    template addLhs() =
      copyIntoKind dest, ArrAtX, info:
        dest.addSubtree a
        addUIntTypedOp dest, ShrX, -1, info:
          addUIntTypedOp dest, CastX, -1, info:
            dest.addSubtree b
          dest.addUIntLit(3)
    copyIntoKind dest, AsgnS, info:
      addLhs()
      addUIntTypedOp dest, if kind == InclS: BitOrX else: BitAndX, 8, info:
        addLhs()
        if kind == ExclS:
          dest.addParLe BitNotX, info
          dest.addUintType(8, info)
        addUIntTypedOp dest, ShlX, 8, info:
          dest.addUIntLit(1, info)
          addUIntTypedOp dest, BitAndX, -1, info:
            dest.addSubtree b
            dest.addUIntLit(7, info)
        if kind == ExclS:
          dest.addParRi()
  if useTemp:
    dest.addParRi()
    c.tempUseBufStack.shrink(oldBufStackLen)

proc trExpr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # Simplify (expr (expr ...)) to (expr (...)) so that our
  # controlflow graph can handle them easily:
  dest.add n
  inc n
  var nestedExpr = 0
  while n.exprKind == ExprX:
    inc n
    inc nestedExpr
  while n.kind != ParRi:
    tr(c, dest, n)
  inc n
  dest.addParRi()
  while nestedExpr > 0:
    skipParRi n
    dec nestedExpr

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor; isTopScope = false) =
  case n.kind
  of DotToken, UnknownToken, EofToken, Ident, Symbol, SymbolDef, IntLit, UIntLit, FloatLit, CharLit, StringLit:
    takeTree dest, n
  of ParLe:
    case n.exprKind
    of NoExpr:
      case n.stmtKind
      of NoStmt:
        case n.typeKind
        of SetT:
          #trSetType(c, dest, n)
          # leave this to nifcgen
          trSons(c, dest, n)
        else:
          trSons(c, dest, n)
      of InclS, ExclS:
        genInclExcl(c, dest, n)
      of CaseS:
        copyInto dest, n:
          while n.kind != ParRi:
            case n.substructureKind
            of OfU:
              copyInto dest, n:
                takeTree dest, n # keep set constructor
                tr(c, dest, n)
            else:
              tr(c, dest, n)
      of LocalDecls:
        trLocal c, dest, n
      of ProcS, FuncS, MacroS, MethodS, ConverterS:
        trProc c, dest, n
      of IteratorS, TemplateS, EmitS, BreakS, ContinueS,
        ForS, IncludeS, ImportS, FromimportS, ImportExceptS,
        ExportS, CommentS,
        PragmasS:
        takeTree dest, n
      of TypeS:
        if isTopScope:
          takeTree dest, n
        else:
          takeTree c.pending, n
      of ScopeS:
        c.typeCache.openScope()
        trSons(c, dest, n)
        c.typeCache.closeScope()
      of StmtsS:
        trSons(c, dest, n, isTopScope = isTopScope)
      else:
        trSons(c, dest, n)
    of SetConstrX:
      genSetConstr(c, dest, n)
    of PlusSetX, MinusSetX, MulSetX, XorSetX, EqSetX, LeSetX, LtSetX, InSetX:
      genSetOp(c, dest, n)
    of CardX:
      genCard(c, dest, n)
    of TypeofX:
      takeTree dest, n
    of DdotX:
      dest.add tagToken("dot", n.info)
      dest.add tagToken("deref", n.info)
      inc n # skip tag
      tr c, dest, n
      dest.addParRi() # deref
      tr c, dest, n
      tr c, dest, n # inheritance depth
      takeParRi dest, n
    of ExprX:
      trExpr c, dest, n
    else:
      trSons(c, dest, n)
  of ParRi:
    bug "unexpected ')' inside"

proc desugar*(n: Cursor; moduleSuffix: string; activeChecks: set[CheckMode]): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix, activeChecks: activeChecks, pending: createTokenBuf())
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  tr c, result, n, isTopScope = true

  assert result[result.len-1].kind == ParRi
  shrink(result, result.len-1)

  result.add c.pending
  result.addParRi()

  c.typeCache.closeScope()
