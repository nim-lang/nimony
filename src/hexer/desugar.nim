# removes abstractions like set ops and ref object constructors

import std / [assertions]
include nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, expreval, xints, builtintypes]
import basics, typekeys

type
  Context = object
    counter: int
    typeCache: TypeCache
    thisModuleSuffix: string
    tempUseBufStack: seq[TokenBuf]

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
    of AtX, PatX, ArrAtX, TupAtX, DotX, DdotX, ParX, AddrX, HaddrX:
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
    error "expected ')', but got: ", n

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trSons(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    while n.kind != ParRi:
      tr(c, dest, n)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc trProc(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  copyInto dest, n:
    let isConcrete = c.typeCache.takeRoutineHeader(dest, n)
    if isConcrete:
      tr(c, dest, n)
    else:
      takeTree dest, n
  c.typeCache.closeScope()

proc addUintType(buf: var TokenBuf; bits: int; info: PackedLineInfo) =
  buf.add tagToken("u", info)
  buf.addIntLit(bits, info)
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
    case size
    of 1, 2, 4, 8:
      dest.addUintType(size * 8, info)
    else:
      dest.add tagToken("array", info)
      dest.addUintType(8, info)
      dest.addIntLit(size, info)
      dest.addParRi()
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
    b = liftTemp(c, dest, bOrig, typ, info)
  else:
    a = aOrig
    b = bOrig
  var err = false
  let size = asSigned(bitsetSizeInBytes(baseType), err)
  assert not err
  case size
  of 1, 2, 4, 8:
    case kind
    of CardX:
      # XXX needs countBits compilerproc
      raiseAssert("unimplemented")
    of LtSetX:
      copyIntoKind dest, AndX, info:
        copyIntoKind dest, EqX, info:
          dest.addSubtree cType
          copyIntoKind dest, BitAndX, info:
            dest.addSubtree cType
            dest.addSubtree a
            copyIntoKind dest, BitNotX, info:
              dest.addSubtree cType
              dest.addSubtree b
          dest.addIntLit(0, info)
        copyIntoKind dest, NeqX, info:
          dest.addSubtree cType
          dest.addSubtree a
          dest.addSubtree b
    of LeSetX:
      copyIntoKind dest, EqX, info:
        dest.addSubtree cType
        copyIntoKind dest, BitAndX, info:
          dest.addSubtree cType
          dest.addSubtree a
          copyIntoKind dest, BitNotX, info:
            dest.addSubtree cType
            dest.addSubtree b
        dest.addIntLit(0, info)
    of EqSetX:
      copyIntoKind dest, EqX, info:
        dest.addSubtree cType
        dest.addSubtree a
        dest.addSubtree b
    of MulSetX:
      copyIntoKind dest, BitAndX, info:
        dest.addSubtree cType
        dest.addSubtree a
        dest.addSubtree b
    of PlusSetX:
      copyIntoKind dest, BitOrX, info:
        dest.addSubtree cType
        dest.addSubtree a
        dest.addSubtree b
    of MinusSetX:
      copyIntoKind dest, BitAndX, info:
        dest.addSubtree cType
        dest.addSubtree a
        copyIntoKind dest, BitNotX, info:
          dest.addSubtree cType
          dest.addSubtree b
    of XorSetX:
      copyIntoKind dest, BitXorX, info:
        dest.addSubtree cType
        dest.addSubtree a
        dest.addSubtree b
    of InSetX:
      # XXX subtract offsets for range types if implemented
      let mask = size * 8 - 1
      copyIntoKind dest, NeqX, info:
        dest.addSubtree cType
        copyIntoKind dest, BitAndX, info:
          dest.addSubtree cType
          dest.addSubtree a
          copyIntoKind dest, ShlX, info:
            dest.addSubtree cType
            copyIntoKind dest, CastX, info:
              dest.addSubtree cType
              dest.addIntLit(1, info)
            copyIntoKind dest, BitAndX, info:
              dest.addUintType(-1, info)
              copyIntoKind dest, CastX, info:
                dest.addUintType(-1, info)
                dest.addSubtree b
              dest.addUIntLit(uint64(mask), info)
        dest.addUIntLit(0, info)
    else:
      raiseAssert("unreachable")
  else:
    # XXX implement
    raiseAssert("unimplemented")
  if useTemp:
    dest.addParRi()
    c.tempUseBufStack.shrink(oldBufStackLen)

proc genSetConstr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  var typ = c.typeCache.getType(n)
  var bytes = evalBitSet(n, typ)
  case bytes.len
  of 0:
    # not constant
    # XXX implement
    raiseAssert("unimplemented")
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
    b = liftTemp(c, dest, bOrig, typ, info)
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
      copyIntoKind dest, ShlX, info:
        dest.addSubtree cType
        copyIntoKind dest, CastX, info:
          dest.addSubtree cType
          dest.addIntLit(1, info)
        copyIntoKind dest, BitAndX, info:
          dest.addUintType(-1, info)
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
        copyIntoKind dest, ShrX, info:
          dest.addUintType(-1, info)
          copyIntoKind dest, CastX, info:
            dest.addUintType(-1, info)
            dest.addParRi()
            dest.addSubtree b
          dest.addIntLit(3)
    copyIntoKind dest, AsgnS, info:
      addLhs()
      copyIntoKind dest, if kind == InclS: BitOrX else: BitAndX, info:
        addLhs()
        if kind == ExclS:
          dest.addParLe BitNotX, info
          dest.addUintType(8, info)
        copyIntoKind dest, ShlX, info:
          dest.addUintType(8, info)
          dest.addUIntLit(1, info)
          copyIntoKind dest, BitAndX, info:
            dest.addUintType(-1, info)
            dest.addSubtree b
            dest.addUIntLit(7, info)
        if kind == ExclS:
          dest.addParRi()
  if useTemp:
    dest.addParRi()
    c.tempUseBufStack.shrink(oldBufStackLen)

proc trNewobjFields(c: var Context; dest: var TokenBuf; n: var Cursor) =
  while n.kind != ParRi:
    if n.substructureKind == KvU:
      copyInto dest, n:
        takeTree dest, n # keep field name
        tr(c, dest, n)
    else:
      tr(c, dest, n)
  inc n # skip ParRi

proc genNewobj(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  let refType = n
  assert refType.typeKind in {RefT, RefobjT}

  let baseType = refType.firstSon
  var refTypeCopy = refType
  let typeKey = takeMangle refTypeCopy
  let typeSym = pool.syms.getOrIncl(typeKey & GeneratedTypeSuffix)

  copyIntoKind dest, ExprX, info:
    copyIntoKind dest, StmtsS, info:
      let tmp = declareTemp(c, dest, refType, info)
      copyIntoKind dest, CastX, info:
        dest.addSubtree refType
        copyIntoKind dest, CallX, info:
          dest.add symToken(pool.syms.getOrIncl("allocFixed.0." & SystemModuleSuffix), info)
          copyIntoKind dest, SizeofX, info:
            dest.add symToken(typeSym, info)
      dest.addParRi() # finish temp declaration
      copyIntoKind dest, AsgnS, info:
        copyIntoKind dest, DerefX, info:
          dest.add symToken(tmp, info)
        copyIntoKind dest, OconstrX, info:
          dest.add symToken(typeSym, info)
          copyIntoKind dest, KvU, info:
            let rcField = pool.syms.getOrIncl(RcField)
            dest.add symdefToken(rcField, info)
            dest.addIntLit(0, info)
          copyIntoKind dest, KvU, info:
            let dataField = pool.syms.getOrIncl(DataField)
            dest.add symdefToken(dataField, info)
            copyIntoKind dest, OconstrX, info:
              dest.addSubtree baseType
              trNewobjFields(c, dest, n)
    # ExprX's expression is the temp:
    dest.add symToken(tmp, info)

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
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
      of IteratorS, TemplateS, TypeS, EmitS, BreakS, ContinueS,
        ForS, CmdS, IncludeS, ImportS, FromS, ImportExceptS,
        ExportS, CommentS,
        PragmasS:
        takeTree dest, n
      of ScopeS:
        c.typeCache.openScope()
        trSons(c, dest, n)
        c.typeCache.closeScope()
      else:
        trSons(c, dest, n)
    of SetConstrX:
      genSetConstr(c, dest, n)
    of PlusSetX, MinusSetX, MulSetX, XorSetX, EqSetX, LeSetX, LtSetX, InSetX, CardX:
      genSetOp(c, dest, n)
    of NewobjX:
      genNewobj(c, dest, n)
    of TypeofX:
      takeTree dest, n
    else:
      trSons(c, dest, n)
  of ParRi:
    raiseAssert "unexpected ')' inside"

proc desugar*(n: Cursor; moduleSuffix: string): TokenBuf =
  var c = Context(counter: 0, typeCache: createTypeCache(), thisModuleSuffix: moduleSuffix)
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  tr c, result, n
  c.typeCache.closeScope()
