#
#
#           Nimony
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A type navigator can recompute the type of an expression.

include nifprelude

import std/tables
import nimony_model, builtintypes, decls, programs

type
  LocalInfo* = object
    kind*: SymKind
    typ*: Cursor
  TypeScope* {.acyclic.} = ref object
    locals: Table[SymId, LocalInfo]
    parent: TypeScope

  TypeCache* = object
    builtins*: BuiltinTypes
    mem: seq[TokenBuf]
    current: TypeScope

proc createTypeCache*(): TypeCache =
  TypeCache(builtins: createBuiltinTypes())

proc registerLocal*(c: var TypeCache; s: SymId; kind: SymKind; typ: Cursor) =
  c.current.locals[s] = LocalInfo(kind: kind, typ: typ)

proc openScope*(c: var TypeCache) =
  c.current = TypeScope(locals: initTable[SymId, LocalInfo](), parent: c.current)

proc closeScope*(c: var TypeCache) =
  c.current = c.current.parent

proc registerParams*(c: var TypeCache; routine: SymId; params: Cursor) =
  if params.kind == ParLe:
    var p = params
    inc p
    while p.kind != ParRi:
      let r = takeLocal(p, SkipFinalParRi)
      registerLocal(c, r.name.symId, ParamY, r.typ)
  c.current.locals[routine] = LocalInfo(kind: ProcY, typ: params)

proc firstSon(n: Cursor): Cursor {.inline.} =
  result = n
  inc result

proc getInitValueImpl(c: var TypeCache; s: SymId): Cursor =
  var it {.cursor.} = c.current
  while it != nil:
    var res = it.locals.getOrDefault(s)
    if res.kind != NoSym:
      if res.kind == ConstY:
        # we know the init value comes after the type:
        skip res.typ
        return res.typ
      else:
        return default(Cursor)
    it = it.parent
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    let local = asLocal(res.decl)
    if local.kind == ConstY:
      return local.val
  return default(Cursor)

proc getInitValue*(c: var TypeCache; s: SymId): Cursor =
  result = getInitValueImpl(c, s)
  var counter = 0
  while counter < 20 and not cursorIsNil(result) and result.kind == Symbol:
    dec counter
    # see if we can resolve it even further:
    let res = getInitValueImpl(c, result.symId)
    if not cursorIsNil(res):
      result = res

proc getTypeImpl(c: var TypeCache; n: Cursor): Cursor =
  result = c.builtins.autoType # to indicate error
  case exprKind(n)
  of NoExpr:
    case n.kind
    of Symbol:
      var it {.cursor.} = c.current
      while it != nil:
        let res = it.locals.getOrDefault(n.symId)
        if res.kind != NoSym:
          return res.typ
        it = it.parent
      let res = tryLoadSym(n.symId)
      if res.status == LacksNothing:
        let local = asLocal(res.decl)
        if local.kind.isLocal:
          result = local.typ
        else:
          let fn = asRoutine(res.decl)
          if isRoutine(fn.kind):
            result = fn.params
          #if isRoutine(symKind(res.decl)):
          #  result = res.decl
      else:
        quit "could not find symbol: " & pool.syms[n.symId]
    of IntLit:
      result = c.builtins.intType
    of UintLit:
      result = c.builtins.uintType
    of CharLit:
      result = c.builtins.charType
    of FloatLit:
      result = c.builtins.floatType
    of StringLit:
      result = c.builtins.stringType
    of ParLe:
      case stmtKind(n)
      of IfS:
        var n = n
        inc n
        inc n # skip `elif`
        skip n # skip condition
        result = getTypeImpl(c, n)
      of CaseS:
        var n = n
        inc n # skip `case`
        skip n # skip selector
        inc n # skip `of`
        skip n # skip set
        result = getTypeImpl(c, n)
      of TryS:
        var n = n
        inc n
        result = getTypeImpl(c, n)
      of BlockS:
        var n = n
        inc n
        skip n # label or DotToken
        result = getTypeImpl(c, n)
      else:
        discard
    else:
      case n.substructureKind
      of RangesU, RangeU:
        result = getTypeImpl(c, n.firstSon)
      of KvU:
        var n = n
        inc n # skip "kv"
        skip n # skip key
        result = getTypeImpl(c, n)
      else: discard
  of AtX, PatX, ArrAtX:
    result = getTypeImpl(c, n.firstSon)
    case typeKind(result)
    of ArrayT:
      inc result # to the element type
    of CstringT:
      result = c.builtins.charType
    else:
      result = c.builtins.autoType # still an error
  of PragmaxX:
    var n = n
    inc n
    skip n # pragmas
    result = getTypeImpl(c, n)
  of DoX:
    # the parameter list that follows `(do)` is actually a good type
    result = getTypeImpl(c, n.firstSon)
  of ExprX:
    var n = n
    inc n # skip "expr"
    while n.kind != ParRi:
      let prev = n
      skip n
      if n.kind == ParRi:
        result = getTypeImpl(c, prev)
  of CallX, CallStrLitX, InfixX, PrefixX, CmdX, HcallX:
    result = getTypeImpl(c, n.firstSon)
    if result.kind == ParLe and result.typeKind == ParamsT:
      skip result # skip "params"
      # return retType
    elif typeKind(result) in {IteratorT, ProctypeT}:
      inc result
      inc result # dot token
      skip result # parameters
  of FalseX, TrueX, AndX, OrX, NotX, DefinedX, DeclaredX, IsmainmoduleX, EqX, NeqX, LeX, LtX,
     EqsetX, LesetX, LtsetX, InsetX,
     CompilesX:
    result = c.builtins.boolType
  of NegX, NegInfX, NanX, InfX:
    result = c.builtins.floatType
  of EnumToStrX, DefaultObjX, DefaultTupX:
    result = c.builtins.stringType
  of SizeofX, CardX, AlignofX, OffsetofX:
    result = c.builtins.intType
  of AddX, SubX, MulX, DivX, ModX, ShlX, ShrX, AshrX, BitandX, BitorX, BitxorX, BitnotX,
     PlusSetX, MinusSetX, MulSetX, XorSetX,
     CastX, ConvX, OconvX, HconvX, DconvX, OconstrX, NewOconstrX, AconstrX, SetConstrX:
    result = n.firstSon
  of ParX, EmoveX:
    result = getTypeImpl(c, n.firstSon)
  of NilX:
    result = c.builtins.nilType
  of DotX, DdotX:
    result = n
    skip result # obj
    result = getTypeImpl(c, result) # typeof(obj.field) == typeof field
  of DerefX, HderefX:
    result = getTypeImpl(c, n.firstSon)
    if typeKind(result) in {RefT, PtrT}:
      inc result
    else:
      result = c.builtins.autoType # still an error
  of QuotedX, OchoiceX, CchoiceX, UnpackX, TypeofX, LowX, HighX, ErrX:
    discard "keep the error type"
  of AddrX, HaddrX:
    let elemType = getTypeImpl(c, n.firstSon)
    var buf = createTokenBuf(4)
    buf.add parLeToken(PtrT, n.info)
    buf.addSubtree elemType
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of CurlyX:
    # should not be encountered but keep this code for now
    let elemType = getTypeImpl(c, n.firstSon)
    var buf = createTokenBuf(4)
    buf.add parLeToken(SetT, n.info)
    buf.addSubtree elemType
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of TupleConstrX:
    var buf = createTokenBuf(4)
    buf.add parLeToken(TupleT, n.info)
    var n = n
    inc n
    while n.kind != ParRi:
      buf.addSubtree getTypeImpl(c, n)
      skip n
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of TupAtX:
    var n = n
    inc n # into tuple
    var tupType = getTypeImpl(c, n)
    skip n # skip tuple expression
    if n.kind == IntLit:
      var idx = pool.integers[n.intId]
      inc tupType # into the tuple type
      while idx > 0:
        skip tupType
        dec idx
      if tupType == "fld":
        let field = asLocal(tupType)
        result = field.typ
      else:
        result = tupType
  of BracketX:
    # should not be encountered but keep this code for now
    let elemType = getTypeImpl(c, n.firstSon)
    var buf = createTokenBuf(4)
    buf.add parLeToken(ArrayT, n.info)
    buf.addSubtree elemType
    var n = n
    var arrayLen = 0
    inc n # skips BracketX
    while n.kind != ParRi:
      skip n
      inc arrayLen
    buf.addIntLit(arrayLen, n.info)
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of DestroyX, CopyX, WasMovedX, SinkhX, TraceX:
    result = c.builtins.voidType
  of DupX:
    result = getTypeImpl(c, n.firstSon)
  of CurlyatX, TabconstrX:
    # error: should have been eliminated earlier
    result = c.builtins.autoType
  of SufX:
    var n = n
    inc n # tag
    skip n # expr
    if n.kind in {Ident, StringLit}:
      case pool.strings[n.litId]
      of "i": result = c.builtins.intType
      of "i8": result = c.builtins.int8Type
      of "i16": result = c.builtins.int16Type
      of "i32": result = c.builtins.int32Type
      of "i64": result = c.builtins.int64Type
      of "u": result = c.builtins.uintType
      of "u8": result = c.builtins.uint8Type
      of "u16": result = c.builtins.uint16Type
      of "u32": result = c.builtins.uint32Type
      of "u64": result = c.builtins.uint64Type
      of "f": result = c.builtins.floatType
      of "f32": result = c.builtins.float32Type
      of "f64": result = c.builtins.float64Type
      of "R": result = c.builtins.stringType
      else: result = c.builtins.autoType

proc getType*(c: var TypeCache; n: Cursor): Cursor =
  getTypeImpl c, n

proc takeRoutineHeader*(c: var TypeCache; dest: var TokenBuf; n: var Cursor): bool =
  # returns false if the routine is generic
  result = true # assume it is concrete
  let sym = n.symId
  for i in 0..<BodyPos:
    if i == ParamsPos:
      c.registerParams(sym, n)
    elif i == TypeVarsPos:
      result = n.substructureKind != TypevarsU
    takeTree dest, n

proc takeLocalHeader*(c: var TypeCache; dest: var TokenBuf; n: var Cursor; kind: SymKind) =
  let name = n.symId
  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  c.registerLocal(name, kind, n)
  takeTree dest, n # type
