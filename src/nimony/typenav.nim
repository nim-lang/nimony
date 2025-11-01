#
#
#           Nimony
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A type navigator can recompute the type of an expression.

import std/assertions
include ".." / lib / nifprelude

import std/tables
from std/strutils import endsWith
import nimony_model, builtintypes, decls, programs

const
  RcField* = "r.0"
  DataField* = "d.0"
  VTableField* = "`vt.0"
  DisplayLenField* = "dl.0."
  DisplayField* = "dy.0."
  MethodsField* = "mt.0."

type
  LocalInfo* = object
    kind*: SymKind
    crossedProc*: int16
    typ*: Cursor
  ScopeKind* = enum
    OtherScope, ProcScope, UnusedScope
  TypeScope* {.acyclic.} = ref object
    locals: Table[SymId, LocalInfo]
    parent: TypeScope
    kind: ScopeKind

  TypeCache* = object
    builtins*: BuiltinTypes
    mem: seq[TokenBuf]
    current: TypeScope

proc createTypeCache*(): TypeCache =
  TypeCache(builtins: createBuiltinTypes())

proc registerLocal*(c: var TypeCache; s: SymId; kind: SymKind; typ: Cursor) =
  c.current.locals[s] = LocalInfo(kind: kind, typ: typ)

proc openScope*(c: var TypeCache; kind = OtherScope) =
  c.current = TypeScope(locals: initTable[SymId, LocalInfo](), parent: c.current, kind: kind)

proc closeScope*(c: var TypeCache) =
  c.current = c.current.parent

iterator currentScopeLocals*(c: var TypeCache): SymId =
  for s, k in c.current.locals:
    if k.kind in {VarY, TvarY, GvarY, LetY, TletY, GletY}:
      yield s

proc registerParams*(c: var TypeCache; routine: SymId; decl, params: Cursor) =
  if params.kind == ParLe:
    var p = params
    inc p
    while p.kind != ParRi:
      let r = takeLocal(p, SkipFinalParRi)
      registerLocal(c, r.name.symId, ParamY, r.typ)
  c.current.locals[routine] = LocalInfo(kind: ProcY, typ: decl)

proc openProcScope*(c: var TypeCache; routine: SymId; decl, params: Cursor) =
  registerLocal(c, routine, ProcY, decl)
  c.current = TypeScope(locals: initTable[SymId, LocalInfo](), parent: c.current, kind: ProcScope)

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

proc getLocalInfo*(c: var TypeCache; s: SymId): LocalInfo =
  var it {.cursor.} = c.current
  var crossedProc = 0
  var compareTo = UnusedScope
  while it != nil:
    var res = it.locals.getOrDefault(s)
    if it.kind == compareTo:
      inc crossedProc
    elif it.kind == ProcScope:
      compareTo = ProcScope
    if res.kind != NoSym:
      res.crossedProc = int16(crossedProc)
      return res
    it = it.parent
  return default(LocalInfo)

proc getInitValue*(c: var TypeCache; s: SymId): Cursor =
  result = getInitValueImpl(c, s)
  var counter = 0
  while counter < 20 and not cursorIsNil(result) and result.kind == Symbol:
    dec counter
    # see if we can resolve it even further:
    let res = getInitValueImpl(c, result.symId)
    if not cursorIsNil(res):
      result = res
    else:
      break

proc lookupSymbol*(c: var TypeCache; s: SymId): Cursor =
  var it {.cursor.} = c.current
  while it != nil:
    let res = it.locals.getOrDefault(s)
    if res.kind != NoSym:
      return res.typ
    it = it.parent
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    let local = asLocal(res.decl)
    if local.kind.isLocal:
      result = local.typ
    else:
      let fn = asRoutine(res.decl)
      if isRoutine(fn.kind):
        result = res.decl
      else:
        # XXX This is not good enough
        result = c.builtins.autoType
      #if isRoutine(symKind(res.decl)):
      #  result = res.decl
  else:
    result = default(Cursor)

proc fetchSymKind*(c: var TypeCache; s: SymId): SymKind =
  var it {.cursor.} = c.current
  while it != nil:
    let res = it.locals.getOrDefault(s)
    if res.kind != NoSym:
      return res.kind
    it = it.parent
  let res = tryLoadSym(s)
  result = if res.status == LacksNothing: res.decl.symKind else: NoSym

proc registerLocals(c: var TypeCache; n: var Cursor) =
  if n.kind == ParLe:
    let k = n.stmtKind
    case k
    of StmtsS:
      inc n
      while n.kind != ParRi:
        registerLocals(c, n)
      inc n
    of LetS, CursorS, VarS, TvarS, TletS, GvarS, GletS:
      inc n
      let name = n.symId
      inc n # name
      skip n # export marker
      skip n # pragmas
      c.registerLocal name, cast[SymKind](k), n
      skip n # type
      skip n # init value
      skipParRi n
    else:
      skip n
  else:
    skip n

type
  GetTypeFlag* = enum
    BeStrict
    SkipAliases

proc getTypeImpl(c: var TypeCache; n: Cursor; flags: set[GetTypeFlag]): Cursor =
  result = c.builtins.autoType # to indicate error
  case exprKind(n)
  of NoExpr:
    case n.kind
    of Symbol:
      result = lookupSymbol(c, n.symId)
      if cursorIsNil(result):
        when defined(debug):
          writeStackTrace()
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
        result = getTypeImpl(c, n, flags)
      of CaseS:
        var n = n
        inc n # skip `case`
        skip n # skip selector
        inc n # skip `of`
        skip n # skip set
        result = getTypeImpl(c, n, flags)
      of TryS:
        var n = n
        inc n
        result = getTypeImpl(c, n, flags)
      of BlockS:
        var n = n
        inc n
        skip n # label or DotToken
        result = getTypeImpl(c, n, flags)
      else:
        discard
    else:
      case n.substructureKind
      of RangesU, RangeU:
        result = getTypeImpl(c, n.firstSon, flags)
      of KvU:
        var n = n
        inc n # skip "kv"
        skip n # skip key
        result = getTypeImpl(c, n, flags)
      else: discard
  of AtX, ArrAtX:
    result = getTypeImpl(c, n.firstSon, flags)
    case typeKind(result)
    of ArrayT, SetT:
      inc result # to the element type
    of CstringT:
      result = c.builtins.charType
    else:
      result = c.builtins.autoType # still an error
  of PatX:
    result = getTypeImpl(c, n.firstSon, flags)
    case typeKind(result)
    of PtrT:
      inc result
      if typeKind(result) in {UarrayT, ArrayT}:
        inc result
      else:
        result = c.builtins.autoType # still an error
    of CstringT:
      result = c.builtins.charType
    else:
      result = c.builtins.autoType # still an error
  of PragmaxX:
    var n = n
    inc n
    skip n # pragmas
    result = getTypeImpl(c, n, flags)
  of DoX:
    # the parameter list that follows `(do)` is actually a good type
    result = getTypeImpl(c, n.firstSon, flags)
  of ExprX:
    var n = n
    inc n # skip "expr"
    while n.kind != ParRi:
      let prev = n
      registerLocals(c, n)
      if n.kind == ParRi:
        result = getTypeImpl(c, prev, flags)
  of CallX, CallStrLitX, InfixX, PrefixX, CmdX, HcallX:
    result = getTypeImpl(c, n.firstSon, flags)
    if result.typeKind in RoutineTypes:
      skipToReturnType result
  of FalseX, TrueX, AndX, OrX, XorX, NotX, DefinedX, DeclaredX, IsmainmoduleX, EqX, NeqX, LeX, LtX,
     EqsetX, LesetX, LtsetX, InsetX, OvfX, CompilesX, InstanceofX, FailedX, IsX:
    result = c.builtins.boolType
  of NegX, NegInfX, NanX, InfX:
    result = c.builtins.floatType
  of EnumToStrX, DefaultObjX, DefaultTupX, DefaultdistinctX, InternalTypeNameX, AstToStrX:
    result = c.builtins.stringType
  of SizeofX, CardX, AlignofX, OffsetofX:
    result = c.builtins.intType
  of AddX, SubX, MulX, DivX, ModX, ShlX, ShrX, AshrX, BitandX, BitorX, BitxorX, BitnotX,
     PlusSetX, MinusSetX, MulSetX, XorSetX,
     CastX, ConvX, HconvX, DconvX, BaseobjX,
     OconstrX, NewobjX, AconstrX, SetConstrX, TupConstrX, NewrefX, DelayX:
    result = n.firstSon
  of ParX, EmoveX, ProccallX:
    result = getTypeImpl(c, n.firstSon, flags)
  of NilX:
    result = c.builtins.nilType
  of DotX, DdotX:
    result = n
    inc result # skip "dot"
    var obj = result
    skip result # obj
    # typeof(obj.field) == typeof field
    if result.kind == Symbol:
      let s = result.symId
      result = lookupSymbol(c, s)
      if cursorIsNil(result):
        if pool.syms[s] == DataField and
            obj.exprKind in {DerefX, HderefX}:
          inc obj
          let typ = getTypeImpl(c, obj, flags)
          if typ.typeKind == RefT:
            result = typ
            inc result
        if cursorIsNil(result):
          when defined(debug):
            writeStackTrace()
          quit "could not find symbol: " & pool.syms[s]
    else:
      result = getTypeImpl(c, result, flags)
  of DerefX, HderefX:
    result = getTypeImpl(c, n.firstSon, flags)
    if typeKind(result) in {RefT, PtrT, MutT, OutT, LentT}:
      inc result
    else:
      assert false, "cannot deref type: " & toString(result, false)
      result = c.builtins.autoType # still an error
  of QuotedX, OchoiceX, CchoiceX, UnpackX, FieldsX, FieldpairsX, TypeofX, LowX, HighX, ErrX,
     InternalFieldPairsX:
    discard "keep the error type"
  of AddrX, HaddrX:
    let elemType = getTypeImpl(c, n.firstSon, flags)
    var buf = createTokenBuf(4)
    buf.add parLeToken(PtrT, n.info)
    buf.addSubtree elemType
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of CurlyX:
    # should not be encountered but keep this code for now
    let elemType = getTypeImpl(c, n.firstSon, flags)
    var buf = createTokenBuf(4)
    buf.add parLeToken(SetT, n.info)
    buf.addSubtree elemType
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of TupX:
    # should not be encountered but keep this code for now
    var buf = createTokenBuf(4)
    buf.add parLeToken(TupleT, n.info)
    var n = n
    inc n
    while n.kind != ParRi:
      var val = n
      if val.substructureKind == KvU:
        inc val
        skip val
      buf.addSubtree getTypeImpl(c, val, flags)
      skip n
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of TupatX:
    var n = n
    inc n # into tuple
    var tupType = getTypeImpl(c, n, flags)
    if tupType.typeKind == TupleT:
      skip n # skip tuple expression
      if n.kind == IntLit:
        var idx = pool.integers[n.intId]
        inc tupType # into the tuple type
        while idx > 0:
          skip tupType
          dec idx
        result = getTupleFieldType(tupType)
    elif BeStrict in flags:
      assert false, "wanted tuple type but got: " & toString(tupType, false)
  of BracketX:
    # should not be encountered but keep this code for now
    let elemType = getTypeImpl(c, n.firstSon, flags)
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
    result = getTypeImpl(c, n.firstSon, flags)
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
      of "R", "T": result = c.builtins.stringType
      of "C": result = c.builtins.cstringType
      else: result = c.builtins.autoType
  of EnvpX:
    result = c.builtins.autoType

  assert result.kind != ParRi, "ParRi for expression: " & toString(n, false)

proc getType*(c: var TypeCache; n: Cursor; flags: set[GetTypeFlag] = {}): Cursor =
  result = getTypeImpl(c, n, flags)
  #assert result.typeKind != AutoT
  if SkipAliases in flags:
    var counter = 20
    while counter > 0 and result.kind == Symbol:
      dec counter
      let d = lookupSymbol(c, result.symId)
      if not cursorIsNil(d) and d.stmtKind == TypeS:
        let decl = asTypeDecl(d)
        result = decl.body
      else:
        break
  assert result.kind != ParRi

proc takeRoutineHeader*(c: var TypeCache; dest: var TokenBuf; decl: Cursor; n: var Cursor): bool =
  # returns false if the routine is generic
  result = true # assume it is concrete
  assert n.kind == SymbolDef, "expected SymbolDef, got: " & toString(n, false)
  let sym = n.symId
  for i in 0..<BodyPos:
    if i == ParamsPos:
      c.registerParams(sym, decl, n)
    elif i == TypevarsPos:
      result = n.substructureKind != TypevarsU
    takeTree dest, n

proc takeLocalHeader*(c: var TypeCache; dest: var TokenBuf; n: var Cursor; kind: SymKind) =
  let name = n.symId
  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  c.registerLocal(name, kind, n)
  takeTree dest, n # type

proc registerLocalPtrOf*(c: var TypeCache; name: SymId; kind: SymKind; elemType: Cursor) =
  var buf = createTokenBuf(4)
  buf.add parLeToken(PtrT, elemType.info)
  buf.addSubtree elemType
  buf.addParRi()
  c.mem.add buf
  c.registerLocal(name, kind, cursorAt(c.mem[c.mem.len-1], 0))
