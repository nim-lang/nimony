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
import ".." / njvl / njvl_model
import nimony_model, builtintypes, decls, programs, typeprops

const
  RcField* = "r.00"
  DataField* = "d.00"
  VTableField* = "vt.00"
  DisplayLenField* = "dl.0"
  DisplayField* = "dy.0"
  MethodsField* = "mt.0"

type
  LocalInfo* = object
    kind*: SymKind
    crossedProc*: int16
    typ*: Cursor
    val*: Cursor
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

proc createTypeCache*(bits: int = 64): TypeCache =
  TypeCache(builtins: createBuiltinTypes(bits))

proc registerLocal*(c: var TypeCache; s: SymId; kind: SymKind; typ: Cursor;
                    val: Cursor = default(Cursor)) =
  c.current.locals[s] = LocalInfo(kind: kind, typ: typ, val: val)

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
    while p.hasMore:
      let r = takeLocal(p, SkipFinalParRi)
      registerLocal(c, r.name.symId, ParamY, r.typ, r.val)
  c.current.locals[routine] = LocalInfo(kind: ProcY, typ: decl)

proc openProcScope*(c: var TypeCache; routine: SymId; decl, params: Cursor) =
  registerLocal(c, routine, ProcY, decl)
  c.current = TypeScope(locals: initTable[SymId, LocalInfo](), parent: c.current, kind: ProcScope)

proc getInitValueImpl(c: var TypeCache; s: SymId): Cursor =
  ## Returns the init value for a constant. Restricted to `ConstY` for callers
  ## that inline values (constant folding); use `getLocalInfo` / `LocalInfo.val`
  ## directly to inspect ordinary let/var/pattern-var initializers.
  var it {.cursor.} = c.current
  while it != nil:
    let res = it.locals.getOrDefault(s)
    if res.kind != NoSym:
      if res.kind == ConstY:
        return res.val
      else:
        return default(Cursor)
    it = it.parent
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    let local = asLocal(res.decl)
    if local.kind == ConstY:
      return local.val
    elif local.kind == EfldY:
      # Enum field values are stored as `(tup <ord> <str>)`; the caller wants
      # the ordinal literal so constant folding can inline it at use sites
      # (e.g. a `case` label emitted by Lengc must be an integer constant).
      result = local.val
      if result.kind == ParLe and result.exprKind == TupX:
        inc result
        return result
      return default(Cursor)
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
    inc counter
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
      n.into:
        while n.hasMore:
          registerLocals(c, n)
    of LetS, CursorS, PatternvarS, VarS, TvarS, TletS, GvarS, GletS:
      inc n
      let name = n.symId
      inc n # name
      skip n, SkipExport # export marker
      skip n, SkipPragmas # pragmas
      let typ = n
      skip n, SkipType # type
      let val = n
      c.registerLocal name, cast[SymKind](k), typ, val
      skip n, SkipValue # init value
      skipParRi n
    else:
      skip n
  else:
    skip n

type
  GetTypeFlag* = enum
    BeStrict
    SkipAliases

proc skipToObjectBody(n: Cursor): Cursor =
  var counter = 20
  result = n
  if result.typeKind in {PtrT, RefT}:
    inc result
  while counter > 0 and result.kind == Symbol:
    dec counter
    let d = getTypeSection(result.symId)
    if d.kind == TypeY:
      result = d.body
      if result.typeKind in {PtrT, RefT}:
        inc result
    else:
      break

proc typeOfField(c: var TypeCache; n: var Cursor; fld: SymId): Cursor =
  if n.substructureKind in {FldU, GfldU}:
    let decl = takeLocal(n, SkipFinalParRi)
    if decl.name.kind == SymbolDef and decl.name.symId == fld:
      result = decl.typ
    else:
      result = default(Cursor)
  elif n.substructureKind == StmtsU:
    n.into:
      while n.hasMore:
        result = typeOfField(c, n, fld)
        if not cursorIsNil(result): return result
    result = default(Cursor)
  elif n.substructureKind == CaseU:
    inc n
    result = typeOfField(c, n, fld) # selector field
    if not cursorIsNil(result): return result
    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:
          skip n, SkipValue # ranges
          n.into:                                # (stmts ...)
            while n.hasMore:
              result = typeOfField(c, n, fld)
              if not cursorIsNil(result): return result
      of ElseU:
        n.into:
          n.into:                                # (stmts ...)
            while n.hasMore:
              result = typeOfField(c, n, fld)
              if not cursorIsNil(result): return result
      else:
        skip n
    skipParRi n
    result = default(Cursor)
  else:
    result = default(Cursor)
    let tk = n.typeKind
    if tk in {ObjectT, TupleT}:
      inc n
      var baseObj = default(Cursor)
      if tk == ObjectT:
        baseObj = n
        skip n # inheritance
      while n.hasMore:
        result = typeOfField(c, n, fld)
        if not cursorIsNil(result): return result
      inc n
      if not cursorIsNil(baseObj):
        var b = skipToObjectBody baseObj
        result = typeOfField(c, b, fld)

proc lookupField*(c: var TypeCache; typ: Cursor; fld: SymId): Cursor =
  var body = skipToObjectBody(typ)
  result = typeOfField(c, body, fld)

proc getTypeImpl(c: var TypeCache; n: Cursor; flags: set[GetTypeFlag]): Cursor

proc tupatType(c: var TypeCache; n: Cursor; flags: set[GetTypeFlag]): Cursor =
  result = c.builtins.autoType # to indicate error
  var n = n
  inc n # into tuple
  var tupType = getTypeImpl(c, n, flags)
  tupType = skipModifier(tupType)
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

proc getTypeImpl(c: var TypeCache; n: Cursor; flags: set[GetTypeFlag]): Cursor =
  result = c.builtins.autoType # to indicate error
  case exprKind(n)
  of NoExpr:
    case n.kind
    of Symbol:
      result = lookupSymbol(c, n.symId)
      if cursorIsNil(result):
        bug "could not find symbol: " & pool.syms[n.symId]
    of IntLit:
      result = c.builtins.intType
    of UIntLit:
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
        # Walk all branches and pick the first non-void branch's type. A
        # branch tagged `(stmts ...)` (e.g. one whose last expression is a
        # `return`/`raise`) yields void; the if-expression's value comes
        # from a sibling branch that does yield.
        var n = n
        inc n # skip `if`
        result = c.builtins.voidType
        while n.kind == ParLe:
          let sub = n.substructureKind
          if sub notin {ElifU, ElseU}: break
          var br = n
          inc br # `elif` or `else`
          if sub == ElifU:
            skip br # condition
          let brType = getTypeImpl(c, br, flags)
          if brType.typeKind != VoidT:
            result = brType
            break
          skip n
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
      of StmtsS, RetS, CommentS:
        result = c.builtins.voidType
      else:
        case njvlKind(n)
        of VV:
          result = getTypeImpl(c, n.firstSon, flags)
        of EtupatV:
          result = tupatType(c, n, flags)
        else: discard
    else:
      # XXX FIXME This can never be true as we know n.kind != ParLe!
      case n.substructureKind
      of RangesU, RangeU:
        result = getTypeImpl(c, n.firstSon, flags)
      else: discard
  of KvX:
    var m = n
    inc m # skip "kv"
    skip m # skip key
    result = getTypeImpl(c, m, flags)
  of AtX, ArratX:
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
    of UarrayT:
      inc result # element type
    of CstringT:
      result = c.builtins.charType
    else:
      result = c.builtins.autoType # still an error
  of PragmaxX:
    var n = n
    inc n
    skip n, SkipPragmas # pragmas
    result = getTypeImpl(c, n, flags)
  of DoX:
    # the parameter list that follows `(do)` is actually a good type
    result = getTypeImpl(c, n.firstSon, flags)
  of ExprX:
    var n = n
    inc n # skip "expr"
    while n.hasMore:
      let prev = n
      registerLocals(c, n)
      if n.kind == ParRi:
        result = getTypeImpl(c, prev, flags)
  of CallX, CallstrlitX, InfixX, PrefixX, CmdX, HcallX, ProccallX:
    result = getTypeImpl(c, n.firstSon, flags)
    if result.typeKind in RoutineTypes:
      skipToReturnType result
  of FalseX, TrueX, AndX, OrX, XorX, NotX, DefinedX, DeclaredX, IsmainmoduleX, EqX, NeqX, LeX, LtX,
     EqsetX, LesetX, LtsetX, InsetX, OvfX, CompilesX, InstanceofX, FailedX, IsX:
    result = c.builtins.boolType
  of NeginfX, NanX, InfX:
    result = c.builtins.floatType
  of EnumtostrX, DefaultobjX, DefaulttupX, DefaultdistinctX, InternalTypeNameX,
     AstToStrX, BindSymNameX:
    result = c.builtins.stringType
  of BindSymX:
    # bindSym returns NimNode. By the time typenav is reached, sem has
    # already rewritten the call to `newSymNode(...)`, so this branch
    # mainly exists for case-completeness.
    result = c.builtins.autoType
  of SizeofX, CardX, AlignofX, OffsetofX:
    result = c.builtins.intType
  of DelayX, Delay0X, SuspendX:
    result = c.builtins.continuationType
  of AddX, SubX, MulX, DivX, ModX, ShlX, ShrX, AshrX, BitandX, BitorX, BitxorX, BitnotX, NegX,
     PlussetX, MinussetX, MulsetX, XorsetX,
     CastX, ConvX, HconvX, DconvX, BaseobjX,
     OconstrX, NewobjX, AconstrX, SetconstrX, TupconstrX, NewrefX:
    result = n.firstSon
  of ParX, EmoveX:
    result = getTypeImpl(c, n.firstSon, flags)
  of NilX:
    result = c.builtins.nilType
  of DotX, DdotX:
    var n = n
    inc n # skip "dot"
    var obj = n
    skip n # object expression
    let fld = n.symId

    var objType = skipToObjectBody skipModifier getTypeImpl(c, obj, flags)

    result = typeOfField(c, objType, fld)
    if cursorIsNil(result):
      if pool.syms[fld] == VTableField:
        # VTableField is a magic internal field for RTTI
        result = c.builtins.vtableType
      elif pool.syms[fld] == DataField and
            obj.exprKind in {DerefX, HderefX}:
        inc obj
        var t = getTypeImpl(c, obj, flags)
        if t.kind == Symbol:
          var counter = 20
          while counter > 0 and t.kind == Symbol:
            dec counter
            let res = tryLoadSym(t.symId)
            if res.status == LacksNothing and res.decl.stmtKind == TypeS:
              let decl = asTypeDecl(res.decl)
              t = decl.body
            else:
              break
        if t.typeKind == RefT:
          result = t
          inc result
      else:
        result = c.builtins.autoType

  of DerefX, HderefX:
    result = getTypeImpl(c, n.firstSon, flags)
    if typeKind(result) == SinkT:
      inc result

    var counter = 20
    while counter > 0 and result.kind == Symbol:
      dec counter
      let d = getTypeSection(result.symId)
      if d.kind == TypeY:
        result = d.body
      else:
        break
    if typeKind(result) in {RefT, PtrT, MutT, OutT, LentT}:
      inc result
    elif typeKind(result) == CstringT:
      result = c.builtins.charType
    else:
      discard "byref param access: type is already the Nim-level type"
  of QuotedX, OchoiceX, CchoiceX, UnpackX, FieldsX, FieldpairsX, TypeofX, LowX, HighX,
     InternalFieldPairsX:
    discard "keep the error type"
  of ErrX:
    # determining the type of `(err)` is not an error by itself:
    result = n
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
    n.into:
      while n.hasMore:
        var val = n
        if val.substructureKind == KvU:
          inc val          # past (kv parle (read-only probe on copy)
          skip val
        buf.addSubtree getTypeImpl(c, val, flags)
        skip n
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of TupatX:
    result = tupatType(c, n, flags)
  of BracketX:
    # should not be encountered but keep this code for now
    let elemType = getTypeImpl(c, n.firstSon, flags)
    var buf = createTokenBuf(4)
    buf.add parLeToken(ArrayT, n.info)
    buf.addSubtree elemType
    var n = n
    var arrayLen = 0
    let info = n.info
    n.into:                 # past BracketX
      while n.hasMore:
        skip n
        inc arrayLen
    buf.addIntLit(arrayLen, info)
    buf.addParRi()
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)
  of DestroyX, CopyX, WasmovedX, SinkhX, TraceX:
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
  of ToClosureX:
    let srcProc = getTypeImpl(c, n.firstSon, flags).asRoutine
    var buf = createTokenBuf()
    copyIntoKind(buf, srcProc.kind, n.info):
      buf.addDotToken() # name
      buf.addDotToken() # exported
      buf.addSubtree srcProc.pattern
      buf.addDotToken() # typevars
      buf.addSubtree srcProc.params
      buf.addSubtree srcProc.retType
      copyIntoKind(buf, PragmasU, srcProc.pragmas.info):
        if srcProc.pragmas.kind != DotToken:
          var n = srcProc.pragmas
          loopInto n:
            buf.addSubtree n
        buf.addParPair(ClosureP)
      buf.addDotToken # effects
    c.mem.add buf
    result = cursorAt(c.mem[c.mem.len-1], 0)

  assert result.hasMore, "ParRi for expression: " & toString(n, false)

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
  assert result.hasMore

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
  let typ = n
  takeTree dest, n # type
  c.registerLocal(name, kind, typ, n)

proc registerLocalPtrOf*(c: var TypeCache; name: SymId; kind: SymKind; elemType: Cursor) =
  var buf = createTokenBuf(4)
  buf.add parLeToken(PtrT, elemType.info)
  buf.addSubtree elemType
  buf.addParRi()
  c.mem.add buf
  c.registerLocal(name, kind, cursorAt(c.mem[c.mem.len-1], 0))
