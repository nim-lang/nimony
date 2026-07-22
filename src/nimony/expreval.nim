#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## expression evaluator for simple constant expressions, not meant to be complete

when defined(nimony):
  {.feature: "untyped".}
  {.feature: "lenientnils".}

import std / assertions

include ".." / lib / nifprelude
import nimony_model, decls, programs, xints, semdata, renderer, builtintypes, typeprops, langmodes
import ".." / lib / symparser

type
  EvalContext* = object
    c: ptr SemContext
    trueValue, falseValue: Cursor
    expectedType: TypeCursor # used as the result type when forwarding
                             # complex const initialisers (e.g. `block:`)
                             # to `executeExpr`. Default-constructed when
                             # no type context is available.
    noExecute*: bool         # when true the evaluator never shells out to
                             # `executeExpr` (which re-runs the full nimony
                             # pipeline in a sub-compile); anything that would
                             # need it yields a "cannot evaluate" result. Used
                             # by callers such as overload resolution that must
                             # stay in-process and cheap.
    keepEnumFields*: bool    # when true an enum constant folds to its field
                             # symbol (the canonical typed value) instead of
                             # collapsing to its bare ordinal, which would drop
                             # the enum type. Used when folding a `static` enum
                             # argument so a `const` alias canonicalizes to the
                             # same value as the literal field it names.

proc isConstBoolValue*(n: Cursor): bool =
  n.exprKind in {TrueX, FalseX}

proc isConstIntValue*(n: Cursor): bool =
  n.isIntLit

proc isConstUIntValue*(n: Cursor): bool =
  n.kind == UIntLit

proc isConstStringValue*(n: Cursor): bool =
  n.isStringLit

proc isConstCharValue*(n: Cursor): bool =
  n.kind == CharLit

proc initEvalContext*(c: ptr SemContext; noExecute = false): EvalContext =
  result = EvalContext(c: c, noExecute: noExecute)

proc error(c: var EvalContext, msg: string, info: PackedLineInfo): Cursor =
  var buf = createTokenBuf(4)
  buf.addParLe nifstreams.ErrT, info
  buf.addDotToken()
  buf.addStrLit msg
  buf.addParRi()
  result = cursorAt(buf, 0)

proc getTrueValue(c: var EvalContext): Cursor =
  if c.trueValue == default(Cursor):
    var buf = createTokenBuf(2)
    buf.addParLe(TrueX, NoLineInfo)
    buf.addParRi()
    c.trueValue = cursorAt(buf, 0)
  result = c.trueValue

proc getFalseValue(c: var EvalContext): Cursor =
  if c.falseValue == default(Cursor):
    var buf = createTokenBuf(2)
    buf.addParLe(FalseX, NoLineInfo)
    buf.addParRi()
    c.falseValue = cursorAt(buf, 0)
  result = c.falseValue

proc getConstOrdinalValue*(val: Cursor): xint =
  case val.kind
  of CharLit:
    result = createXint val.uoperand
  of IntLit:
    result = createXint pool.integers[val.intId]
  of UIntLit:
    result = createXint pool.uintegers[val.uintId]
  of OpenTagKind:
    case val.exprKind
    of FalseX:
      result = createXint(0'i64)
    of TrueX:
      result = createXint(1'i64)
    else:
      result = createNaN()
  else:
    result = createNaN()

proc singleToken*(c: var EvalContext; tok: PackedToken): Cursor =
  var buf = createTokenBuf(1)
  buf.add tok
  result = cursorAt(buf, 0)

proc stringValue(c: var EvalContext; s: string; info: PackedLineInfo): Cursor {.inline.} =
  var buf = createTokenBuf(2)
  buf.addStrLit(s, info)
  result = cursorAt(buf, 0)

proc intValue(c: var EvalContext; i: int64; info: PackedLineInfo): Cursor {.inline.} =
  var buf = createTokenBuf(2)
  buf.addIntLit(i, info)
  result = cursorAt(buf, 0)

proc uintValue(c: var EvalContext; u: uint64; info: PackedLineInfo): Cursor {.inline.} =
  var buf = createTokenBuf(2)
  buf.addUIntLit(u, info)
  result = cursorAt(buf, 0)

proc floatValue(c: var EvalContext; f: float; info: PackedLineInfo): Cursor {.inline.} =
  var buf = createTokenBuf(2)
  buf.addFloatLit(f, info)
  result = cursorAt(buf, 0)

proc charValue(c: var EvalContext; ch: char; info: PackedLineInfo): Cursor {.inline.} =
  var buf = createTokenBuf(2)
  buf.addCharLit(ch, info)
  result = cursorAt(buf, 0)

proc boolValue(c: var EvalContext; val: bool): Cursor {.inline.} =
  if val:
    result = getTrueValue(c)
  else:
    result = getFalseValue(c)

template error(msg: string; info: PackedLineInfo) {.dirty.} =
  result = c.error(msg, info)

template cannotEval(n: Cursor) {.dirty.} =
  result = c.error("cannot evaluate expression at compile time: " & asNimCode(n), n.info)

proc eval*(c: var EvalContext; n: var Cursor): Cursor

proc findObjectField(objType: Cursor; fieldSym: SymId; typ: var Cursor; exported: var bool): bool

proc evalCall(c: var EvalContext; n: Cursor): Cursor =
  var callee = n
  inc callee
  if callee.kind != Symbol:
    # Only explicit generic instantiation `(at T U ...)` needs the sub-compile.
    # Other non-symbol callees (e.g. infix `shl` before overload resolution)
    # are handled elsewhere or via `cannotEval` + `semEnumOrdinalValue`.
    if callee.exprKind != AtX:
      cannotEval(n)
      return
    if c.c == nil or c.c.executeExpr == nil or c.noExecute:
      cannotEval(n)
      return
    var resultBuf = createTokenBuf(12)
    let retType =
      if not cursorIsNil(c.expectedType): skipModifier(c.expectedType)
      else: c.c[].types.autoType
    let errorMsg = c.c.executeExpr(c.c[], n, retType, resultBuf, n.info)
    if errorMsg.len == 0:
      result = cursorAt(resultBuf, 0)
    else:
      result = c.error("cannot evaluate expression at compile time: " & asNimCode(n) & "\n\n" & errorMsg, n.info)
    return
  let res = tryLoadSym(callee.symId)
  if res.status != LacksNothing or not isRoutine(res.decl.symKind):
    cannotEval(n)
    return
  let routine = asRoutine(res.decl)
  var op = ""
  var pragmas = routine.pragmas
  if pragmas.substructureKind == PragmasU:
    pragmas.peekInto:
      while pragmas.hasMore:
        var prag = pragmas
        if prag.pragmaKind == SemanticsP:
          inc prag
          if prag.isIdent or prag.isStringLit:
            op = pool.strings[prag.litId]
            break
        skip pragmas
  var args = n
  args.peekInto:
    skip args # the callee
    case op
    of "string.&":
      let a = eval(c, args)
      let b = eval(c, args)
      if a.kind != StrLitKind or b.kind != StrLitKind or args.hasMore:
        cannotEval(n)
        return
      let val = pool.strings[a.litId] & pool.strings[b.litId]
      result = stringValue(c, val, n.info)
    of "string.==":
      let a = eval(c, args)
      let b = eval(c, args)
      if a.kind != StrLitKind or b.kind != StrLitKind or args.hasMore:
        cannotEval(n)
        return
      let val = pool.strings[a.litId] == pool.strings[b.litId]
      result = boolValue(c, val)
    of "string.len":
      let a = eval(c, args)
      if a.kind != StrLitKind or args.hasMore:
        cannotEval(n)
        return
      let val = pool.strings[a.litId].len
      result = intValue(c, val, n.info)
    else:
      if c.c == nil or c.c.executeExpr == nil or c.noExecute:
        cannotEval(n)
        return
      # Forward args to `executeExpr` verbatim. Running `eval` here would strip
      # distinct/conversion wrappers (e.g. `TagId(1)` → `1`), and the sub-compile
      # would then fail to match the callee's formal parameter types.
      # `executeExpr` re-runs the full nimony pipeline and can resolve constants
      # itself via `rewriteSymsToIdents`.
      var evaluatedCall = createTokenBuf(16)
      evaluatedCall.addParLe CallS, n.info
      evaluatedCall.addSymUse routine.name.symId, n.info
      while args.hasMore:
        evaluatedCall.takeTree args
      evaluatedCall.addParRi()

      var resultBuf = createTokenBuf(12)
      assert c.c.executeExpr != nil
      # Prefer the routine's concrete return type so nested calls inside a
      # distinct conversion (e.g. `Answer(int.fourtytwo)`) are not serialised
      # with the outer const's type. Keep `expectedType` for generic return
      # types such as `@[]` → `newSeqUninit[T](0)` where T comes from context.
      var retType = skipModifier(routine.retType)
      if retType.typeKind == AutoT or containsGenericParams(retType):
        if not cursorIsNil(c.expectedType):
          retType = skipModifier(c.expectedType)
      let errorMsg = c.c.executeExpr(c.c[], cursorAt(evaluatedCall, 0),
                                     retType, resultBuf, n.info)
      if errorMsg.len == 0:
        result = cursorAt(resultBuf, 0)
      else:
        result = c.error("cannot evaluate expression at compile time: " & asNimCode(n) & "\n\n" & errorMsg, n.info)

template evalOrdBinOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  let orig = n
  var isSigned = false
  var a = createNaN()
  var b = createNaN()
  n.into:
    isSigned = n.typeKind == IntT
    skip n, SkipType # type
    a = getConstOrdinalValue propagateError eval(c, n)
    b = getConstOrdinalValue propagateError eval(c, n)
  if not isNaN(a) and not isNaN(b):
    let rx = opr(a, b)
    var err = false
    if isSigned:
      let ri = asSigned(rx, err)
      if err:
        error "expression overflow at compile time: " & asNimCode(orig), orig.info
      else:
        result = intValue(c, ri, orig.info)
    else:
      let ru = asUnsigned(rx, err)
      if err:
        error "expression overflow at compile time: " & asNimCode(orig), orig.info
      else:
        result = uintValue(c, ru, orig.info)
  else:
    cannotEval orig

template evalFloatBinOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  let orig = n
  var a = default(Cursor)
  var b = default(Cursor)
  n.into:
    skip n, SkipType # type
    a = propagateError eval(c, n)
    b = propagateError eval(c, n)
  if a.kind == FloatLit and b.kind == FloatLit:
    let rf = opr(pool.floats[a.floatId], pool.floats[b.floatId])
    result = floatValue(c, rf, orig.info)
  else:
    cannotEval orig

template evalCmpOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  let orig = n
  n.into:
    let t = n
    skip n, SkipType # type
    if t.typeKind == FloatT:
      let a = propagateError eval(c, n)
      let b = propagateError eval(c, n)
      if a.kind == FloatLit and b.kind == FloatLit:
        let rf = opr(pool.floats[a.floatId], pool.floats[b.floatId])
        result = boolValue(c, rf)
      else:
        cannotEval orig
    else:
      let a = getConstOrdinalValue propagateError eval(c, n)
      let b = getConstOrdinalValue propagateError eval(c, n)
      if not isNaN(a) and not isNaN(b):
        let rx = opr(a, b)
        result = boolValue(c, rx)
      else:
        cannotEval orig

template evalBinOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  var t = n
  inc t
  if t.typeKind == FloatT:
    evalFloatBinOp(c, n, opr)
  else:
    evalOrdBinOp(c, n, opr)

template evalOrdUnOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  let orig = n
  var isSigned = false
  var a = createNaN()
  n.into:
    isSigned = n.typeKind == IntT
    skip n, SkipType # type
    a = getConstOrdinalValue propagateError eval(c, n)
  if not isNaN(a):
    let rx = opr(a)
    var err = false
    if isSigned:
      let ri = asSigned(rx, err)
      if err:
        error "expression overflow at compile time: " & asNimCode(orig), orig.info
      else:
        result = intValue(c, ri, orig.info)
    else:
      let ru = asUnsigned(rx, err)
      if err:
        error "expression overflow at compile time: " & asNimCode(orig), orig.info
      else:
        result = uintValue(c, ru, orig.info)
  else:
    cannotEval orig

template evalFloatUnOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  let orig = n
  var a = default(Cursor)
  n.into:
    skip n, SkipType # type
    a = propagateError eval(c, n)
  if a.kind == FloatLit:
    let rf = opr(pool.floats[a.floatId])
    result = floatValue(c, rf, orig.info)
  else:
    cannotEval orig

template evalUnOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  var t = n
  inc t
  if t.typeKind == FloatT:
    evalFloatUnOp(c, n, opr)
  else:
    evalOrdUnOp(c, n, opr)

template evalShiftOp(c0: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  let orig = n
  var isSigned = false
  var bits = c0.c.g.config.bits
  var a = createNaN()
  var b = createNaN()
  n.into:
    isSigned = n.typeKind == IntT
    case n.typeKind
    of IntT, UIntT:
      n.into:
        bits = typebits(n.load)
        while n.hasMore: skip n
    else:
      error "expected int or uint type for shift operation, got: " & typeToString(n), n.info
    a = getConstOrdinalValue propagateError eval(c0, n)
    b = getConstOrdinalValue propagateError eval(c0, n)
  if not isNaN(a) and not isNaN(b):
    var err = false
    var operand = asSigned(b, err)
    if err or operand > high(int).int64:
      error "expression overflow at compile time: " & asNimCode(orig), orig.info
    let rx = mask(opr(a, operand.int), bits, isSigned)
    if isSigned:
      let ri = asSigned(rx, err)
      if err:
        error "expression overflow at compile time: " & asNimCode(orig), orig.info
      else:
        result = intValue(c0, ri, orig.info)
    else:
      let ru = asUnsigned(rx, err)
      if err:
        error "expression overflow at compile time: " & asNimCode(orig), orig.info
      else:
        result = uintValue(c0, ru, orig.info)
  else:
    cannotEval orig

template evalBitnot(c0: var EvalContext; n: var Cursor) {.dirty.} =
  let orig = n
  var isSigned = false
  var bits = c0.c.g.config.bits
  var a = createNaN()
  n.into:
    isSigned = n.typeKind == IntT
    case n.typeKind
    of IntT, UIntT:
      n.into:
        bits = typebits(n.load)
        while n.hasMore: skip n
    else:
      error "expected int or uint type for shl, got: " & typeToString(n), n.info
    a = getConstOrdinalValue propagateError eval(c, n)
  if not isNaN(a):
    var err = false
    let rx = mask(not a, bits, isSigned)
    if isSigned:
      let ri = asSigned(rx, err)
      if err:
        error "expression overflow at compile time: " & asNimCode(orig), orig.info
      else:
        result = intValue(c, ri, orig.info)
    else:
      let ru = asUnsigned(rx, err)
      if err:
        error "expression overflow at compile time: " & asNimCode(orig), orig.info
      else:
        result = uintValue(c, ru, orig.info)
  else:
    cannotEval orig

proc intToToken(result: var TokenBuf; x: int; typ: Cursor) =
  case typ.typeKind
  of IT:
    result.addIntLit x
  of UT:
    result.addUIntLit uint x
  of CT:
    result.addCharLit(char x, NoLineInfo)
  else:
    var hasError = true
    if typ.isSymbol:
      let sym = tryLoadSym(typ.symId)
      if sym.status == LacksNothing:
        var local = asTypeDecl(sym.decl)
        if local.kind == TypeY and local.body.typeKind in {EnumT, HoleyEnumT, AnumT}:
          hasError = false
          result.addIntLit x
    if hasError:
      assert false, "Got unexpected type: " & toString(typ)

proc bitSetToTokens(result: var TokenBuf; x: seq[uint8]; elementTyp: Cursor; info: PackedLineInfo) =
  result.addParLe SetconstrX, info
  result.copyInto(TagId(SetT), NoLineInfo):
    result.addSubtree elementTyp

  var start = -1
  for i in 0 ..< x.len:
    for j in 0..7:
      let val = i * 8 + j
      if (x[i] and (1'u8 shl j)) == 0:
        if start != -1:
          if val - start < 5:
            for k in start ..< val:
              result.intToToken k, elementTyp
          else:
            result.addParLe RangeU
            result.intToToken start, elementTyp
            result.intToToken (val - 1), elementTyp
            result.addParRi
          start = -1
      else:
        if start == -1:
          start = val

  result.addParRi

proc evalBitSetImpl(n, typ: Cursor): seq[uint8]

proc evalOrdinal(c: ptr SemContext, n: Cursor): xint

proc evalInSet(c: var EvalContext; n: var Cursor): Cursor =
  var a = default(Cursor)
  var b = createNaN()
  n.into:
    assert n.typeKind == SetT
    skip n # skip type
    a = eval(c, n)
    b = evalOrdinal(nil, n)
    skip n # skips b
  assert a.exprKind == SetconstrX, "got " & toString(a)

  var isInSet = false
  a.peekInto:
    skip a # skip set type
    while a.hasMore:
      if a.substructureKind == RangeU:
        var xa = createNaN()
        var xb = createNaN()
        a.into:
          xa = evalOrdinal(nil, a)
          skip a
          xb = evalOrdinal(nil, a)
          skip a
        if b >= xa and b <= xb:
          isInSet = true
          break
      else:
        let xa = evalOrdinal(nil, a)
        if xa == b:
          isInSet = true
          break
        skip a

  result = boolValue(c, isInSet)

proc countSetBits(x: uint8): uint8 {.inline.} =
  # Previously realised via a 256-entry lookup table built in a `const block:`
  # but that forced `expreval.eval` to shell out to `executeExpr` at sem time
  # (the only const on this path, ~6s per clean rebuild). A straightforward
  # per-byte formula is fine for set cardinality.
  ( x and 0b00000001'u8) +
    ((x and 0b00000010'u8) shr 1) +
    ((x and 0b00000100'u8) shr 2) +
    ((x and 0b00001000'u8) shr 3) +
    ((x and 0b00010000'u8) shr 4) +
    ((x and 0b00100000'u8) shr 5) +
    ((x and 0b01000000'u8) shr 6) +
    ((x and 0b10000000'u8) shr 7)

proc bitSetCard(x: seq[uint8]): BiggestInt =
  result = 0
  for it in x:
    result.inc int(countSetBits(it))

proc evalCardSet(c: var EvalContext; n: var Cursor): Cursor =
  let info = n.info
  var a = default(Cursor)
  n.into:
    assert n.typeKind == SetT
    skip n # skip type
    a = eval(c, n)

  assert a.exprKind == SetconstrX, "got " & toString(a)
  var typeA = a
  inc typeA

  let setA = evalBitSetImpl(a, typeA)
  result = intValue(c, bitSetCard(setA), info)

proc evalSetOp(c: var EvalContext; n: var Cursor; op: ExprKind): Cursor =
  let info = n.info
  var elementTyp = default(Cursor)
  var a = default(Cursor)
  var b = default(Cursor)
  n.into:
    assert n.typeKind == SetT
    elementTyp = n
    inc elementTyp
    skip n # skip type
    a = eval(c, n)
    b = eval(c, n)
  assert a.exprKind == SetconstrX, "got " & toString(a)
  assert b.exprKind == SetconstrX, "got " & toString(b)
  var typeA = a
  inc typeA
  var typeB = b
  inc typeB
  assert sameTrees(typeA, typeB)  # must be the same type
  let setA = evalBitSetImpl(a, typeA)
  let setB = evalBitSetImpl(b, typeB)
  assert setA.len == setB.len
  var setRes = newSeq[uint8](setA.len)
  case op
  of PlussetX:
    for i in 0 ..< setA.len:
      setRes[i] = setA[i] or setB[i]
  of MinussetX:
    for i in 0 ..< setA.len:
      setRes[i] = setA[i] and not setB[i]
  of XorsetX:
    for i in 0 ..< setA.len:
      setRes[i] = setA[i] xor setB[i]
  of MulsetX:
    for i in 0 ..< setA.len:
      setRes[i] = setA[i] and setB[i]
  else:
    assert false, "unexpected operation: " & $op

  var buf = createTokenBuf()
  buf.bitSetToTokens(setRes, elementTyp, info)
  result = cursorAt(buf, 0)

proc evalCast(c: var EvalContext; typ, val, nOrig: Cursor): Cursor =
  let targetType = toTypeImpl(typ)
  let dtk = targetType.typeKind
  if dtk == FloatT:
    if val.kind == FloatLit:
      result = val
    elif val.isIntLit:
      result = floatValue(c, cast[float64](pool.integers[val.intId]), nOrig.info)
    elif val.kind == UIntLit:
      result = floatValue(c, cast[float64](pool.uintegers[val.uintId]), nOrig.info)
    else:
      cannotEval nOrig
  elif dtk in {IntT, UIntT}:
    if val.kind == FloatLit:
      if dtk == IntT:
        result = intValue(c, cast[int64](pool.floats[val.floatId]), nOrig.info)
      else:
        result = uintValue(c, cast[uint64](pool.floats[val.floatId]), nOrig.info)
    else:
      let x = getConstOrdinalValue(val)
      if isNaN(x):
        cannotEval nOrig
      else:
        var err = false
        if dtk == IntT:
          let i = asSigned(x, err)
          if err: cannotEval nOrig
          else: result = intValue(c, i, nOrig.info)
        else:
          let u = asUnsigned(x, err)
          if err: cannotEval nOrig
          else: result = uintValue(c, u, nOrig.info)
  elif dtk == CharT:
    let x = getConstOrdinalValue(val)
    if isNaN(x):
      cannotEval nOrig
    else:
      var err = false
      let ch = asUnsigned(x, err)
      if err or ch >= 256u:
        cannotEval nOrig
      else:
        result = charValue(c, char(ch), nOrig.info)
  elif dtk == BoolT:
    let x = getConstOrdinalValue(val)
    if isNaN(x):
      cannotEval nOrig
    else:
      result = boolValue(c, x != zero())
  elif dtk in {EnumT, HoleyEnumT, AnumT}:
    let x = getConstOrdinalValue(val)
    if isNaN(x):
      cannotEval nOrig
    else:
      result = val
  elif dtk in {PointerT, PtrT, RefT, CstringT}:
    if val.exprKind == NilX:
      result = val
    elif val.exprKind == AddrX:
      # `cast[ptr U](addr X)` is a pure pointer retag at compile time —
      # preserve the cast wrapper so the new (declared) pointer type flows
      # to codegen. Lengc turns it into `(U*)&X`, which C accepts as a
      # constant initializer for a static.
      result = nOrig
    else:
      cannotEval nOrig
  else:
    cannotEval nOrig

proc eval*(c: var EvalContext; n: var Cursor): Cursor =
  result = default(Cursor)
  template propagateError(r: Cursor): Cursor =
    let val = r
    if val.isTagLit and val.tagId == nifstreams.ErrT:
      return val
    else:
      val
  case n.kind
  of Ident:
    error "cannot evaluate undeclared ident: " & pool.strings[n.litId], n.info
    inc n
  of Symbol:
    let symId = n.symId
    let info = n.info
    let symCursor = n
    inc n
    let sym = tryLoadSym(symId)
    if sym.status == LacksNothing:
      var local = asLocal(sym.decl)
      case local.kind
      of ConstY:
        return eval(c, local.val)
      of EfldY:
        if c.keepEnumFields:
          # keep the field symbol: it is the canonical typed value of its enum
          # type, so folding stops here instead of losing the type to `inc`.
          return symCursor
        inc local.val # takes the first counter field
        return eval(c, local.val)
      else: discard
    error "cannot evaluate symbol at compile time: " & pool.syms[symId], info
  of StrLitKind, CharLit, IntLit, UIntLit, FloatLit:
    result = n
    inc n
  of OpenTagKind:
    let exprKind = n.exprKind
    case exprKind
    of TrueX, FalseX, NanX, InfX, NeginfX, NilX:
      result = n
      skip n
    of AndX:
      n.peekInto:
        let a = propagateError eval(c, n)
        if a.exprKind == FalseX:
          result = a
        elif a.exprKind != TrueX:
          error "expected bool for operand of `and` but got: " & asNimCode(a), n.info
          return
        else:
          let b = propagateError eval(c, n)
          if not isConstBoolValue(b):
            error "expected bool for operand of `and` but got: " & asNimCode(b), n.info
            return
          result = b
      return result
    of OrX:
      n.peekInto:
        let a = propagateError eval(c, n)
        if a.exprKind == TrueX:
          result = a
        elif a.exprKind != FalseX:
          error "expected bool for operand of `or` but got: " & asNimCode(a), n.info
          return
        else:
          let b = propagateError eval(c, n)
          if not isConstBoolValue(b):
            error "expected bool for operand of `or` but got: " & asNimCode(b), n.info
            return
          result = b
      return result
    of NotX:
      n.into:
        let a = propagateError eval(c, n)
        if a.exprKind == TrueX:
          result = c.getFalseValue()
        elif a.exprKind == FalseX:
          result = c.getTrueValue()
        else:
          error "expected bool for operand of `not` but got: " & asNimCode(a), n.info
          return
      return result
    of SufX:
      # we only need raw value
      n.peekInto:
        result = n
    of ConvX, HconvX:
      let nOrig = n
      var isDistinct = false
      var targetType = default(Cursor)
      var val = default(Cursor)
      n.into:
        var typ = skipDistinct(n, isDistinct)
        targetType = toTypeImpl(typ)
        skip n
        val = propagateError eval(c, n)
      if isDistinct:
        result = val
        return
      if targetType.typeKind == CstringT and val.isStringLit:
        result = val
      elif targetType.typeKind == FloatT:
        if val.kind == FloatLit:
          result = val
        else:
          # treats it as an ordinal value
          let x = getConstOrdinalValue(val)
          let f = toFloat64(x)
          result = floatValue(c, f, nOrig.info)
      elif targetType.typeKind == UIntT:
        let x = getConstOrdinalValue(val)
        var err = false
        let u = asUnsigned(x, err)
        if err:
          cannotEval nOrig
        else:
          result = uintValue(c, u, nOrig.info)
      elif targetType.typeKind == IntT:
        let x = getConstOrdinalValue(val)
        var err = false
        let i = asSigned(x, err)
        if err:
          cannotEval nOrig
        else:
          result = intValue(c, i, nOrig.info)
      elif targetType.typeKind == CharT:
        let x = getConstOrdinalValue(val)
        var err = false
        let ch = asUnsigned(x, err)
        if err or ch >= 256u:
          cannotEval nOrig
        else:
          result = charValue(c, char(ch), nOrig.info)
      elif targetType.typeKind in {EnumT, HoleyEnumT, AnumT}:
        let x = getConstOrdinalValue(val)
        if isNaN(x):
          cannotEval nOrig
        else:
          result = val
      else:
        # other conversions not implemented
        cannotEval nOrig
    of CastX:
      let nOrig = n
      var typ = default(Cursor)
      var val = default(Cursor)
      n.into:
        typ = n
        skip n
        val = propagateError eval(c, n)
      result = evalCast(c, typ, val, nOrig)
    of DconvX:
      n.into:
        skip n, SkipType # type
        result = eval(c, n)
    of ExprX:
      let orig = n
      var foldable = true
      n.peekInto:
        while n.hasMore and n.stmtKind == StmtsS:
          var inner = n
          var innerIsEmpty = false
          inner.peekInto:
            innerIsEmpty = not inner.hasMore
          if innerIsEmpty:   # empty `(stmts)` → skip it
            skip n
          else:
            foldable = false
            break
        if foldable and n.hasMore:
          result = propagateError eval(c, n)   # the trailing value
          if n.hasMore:
            foldable = false
        else:
          foldable = false
      if not foldable:
        cannotEval orig
    of NegX:
      evalUnOp(c, n, `-`)
    of MulX:
      evalBinOp(c, n, `*`)
    of AddX:
      evalBinOp(c, n, `+`)
    of SubX:
      evalBinOp(c, n, `-`)
    of DivX:
      var t = n
      inc t
      if t.typeKind == FloatT:
        evalFloatBinOp(c, n, `/`)
      else:
        evalOrdBinOp(c, n, `div`)
    of ModX:
      evalOrdBinOp(c, n, `mod`)
    of BitorX:
      evalOrdBinOp(c, n, `or`)
    of BitandX:
      evalOrdBinOp(c, n, `and`)
    of BitxorX:
      evalOrdBinOp(c, n, `xor`)
    of BitnotX:
      evalBitnot(c, n)
    of ShlX:
      evalShiftOp(c, n, `shl`)
    of ShrX:
      var typ = n
      inc typ
      if typ.typeKind == IntT:
        error "logical right shift not implemented for signed integers", n.info
      # for uints, ashr and shr are the same
      evalShiftOp(c, n, `shr`)
    of AshrX:
      # xints.shr keeps the sign the same, so has ashr behavior for signed ints
      evalShiftOp(c, n, `shr`)
    of EqX:
      evalCmpOp(c, n, `==`)
    of LeX:
      evalCmpOp(c, n, `<=`)
    of LtX:
      evalCmpOp(c, n, `<`)
    of IsmainmoduleX:
      skip n
      if c.c == nil:
        cannotEval n
      else:
        let val = IsMain in c.c.moduleFlags
        result = boolValue(c, val)
    of AconstrX, SetconstrX, TupconstrX,
        BracketX, CurlyX, TupX:
      var buf = createTokenBuf(16)
      copyInto buf, n:
        if exprKind in {AconstrX, SetconstrX, TupconstrX}:
          # add type
          takeTree buf, n
        while n.hasMore:
          if (exprKind == SetconstrX and n.substructureKind == RangeU) or
             (exprKind == AconstrX and n.substructureKind == KvU):
            copyInto buf, n:
              var a = propagateError eval(c, n)
              buf.addSubtree a
              var b = propagateError eval(c, n)
              buf.addSubtree b
          elif exprKind == TupconstrX:
            let isKv = n.substructureKind == KvU
            if isKv:
              n.into:
                skip n # key
                let elem = propagateError eval(c, n)
                buf.addSubtree elem
            else:
              let elem = propagateError eval(c, n)
              buf.addSubtree elem
          else:
            let elem = propagateError eval(c, n)
            buf.addSubtree elem
      result = cursorAt(buf, 0)
    of OconstrX:
      # an already-evaluated object literal: re-emit verbatim, evaluating
      # each field's value. This path is taken on second sem passes that
      # see the value produced by `annotateConstantType`.
      var buf = createTokenBuf(16)
      copyInto buf, n:
        var objBody = skipModifier(n)
        if objBody.isSymbol:
          let res = tryLoadSym(objBody.symId)
          if res.status == LacksNothing:
            objBody = asTypeDecl(res.decl).body
        takeTree buf, n # type
        let savedExpected = c.expectedType
        while n.hasMore:
          if n.substructureKind == KvU:
            copyInto buf, n:
              var fieldSym = SymId(0)
              if n.isSymbol:
                fieldSym = n.symId
              buf.takeToken n # field sym/ident
              var fieldType = default(Cursor)
              var fieldExported = false
              if findObjectField(objBody, fieldSym, fieldType, fieldExported):
                c.expectedType = fieldType
              let v = propagateError eval(c, n)
              c.expectedType = savedExpected
              buf.addSubtree v
              if n.hasMore:
                # optional inheritance level
                buf.takeToken n
          else:
            cannotEval n
            return
      result = cursorAt(buf, 0)
    of AddrX:
      # Pass-through: `addr X` folds to itself, preserving the inner
      # symbol/path verbatim. Recursing into `eval` would replace a `ConstY`
      # symbol with its initializer (see the Symbol arm above) — losing the
      # very reference we want to address. Leng accepts `&staticSym` as a
      # constant initializer for a static, so no further lowering is needed
      # to make `const p = addr someConst` work end-to-end.
      # `HaddrX` is the hidden mutable-borrow form (yields `var T`/MutT, not
      # `ptr T`) and intentionally not handled here.
      result = n
      skip n
    of CallKinds:
      result = evalCall(c, n)
      skip n
    of SizeofX:
      let s = c.c.semGetSize(c.c[], n.firstSon)
      var err = false
      let value = asSigned(s, err)
      if err:
        cannotEval n
      else:
        result = intValue(c, value, n.info)
      skip n
    of PlussetX, MinussetX, XorsetX, MulsetX:
      result = evalSetOp(c, n, n.exprKind)
    of InsetX:
      result = evalInSet(c, n)
    of CardX:
      result = evalCardSet(c, n)
    else:
      if n.tagId == nifstreams.ErrT:
        result = n
        skip n
      elif (n.stmtKind == BlockS or n.stmtKind == StmtsS) and
           not cursorIsNil(c.expectedType) and c.c != nil and
           c.c.executeExpr != nil and not c.noExecute:
        # Const initialisers such as
        #   `const x: T = block: ...; var ...; for ...; expr`
        # cannot be folded by the in-process evaluator. Forward the whole
        # expression to a sub-compile via `executeExpr`, which builds a tiny
        # wrapper program that runs the block and serialises its result.
        let info = n.info
        var resultBuf = createTokenBuf(12)
        let exprStart = n
        let errMsg = c.c.executeExpr(c.c[], exprStart, skipModifier(c.expectedType), resultBuf, info)
        skip n
        if errMsg.len == 0:
          result = cursorAt(resultBuf, 0)
        else:
          result = c.error("cannot evaluate expression at compile time: " &
            asNimCode(exprStart) & "\n\n" & errMsg, info)
      else:
        cannotEval n
  else:
    cannotEval n

proc evalExpr*(c: var SemContext, n: var Cursor;
               expectedType: TypeCursor = default(Cursor);
               keepEnumFields = false): TokenBuf =
  ## When `keepEnumFields` is set, an enum constant folds to its field symbol
  ## rather than its bare ordinal, preserving the enum type.
  var ec = initEvalContext(addr c)
  ec.expectedType = expectedType
  ec.keepEnumFields = keepEnumFields
  let val = eval(ec, n)
  result = createTokenBuf(val.span)
  result.addSubtree val

proc evalOrdinal(c: ptr SemContext, n: Cursor): xint =
  var ec = initEvalContext(c)
  var n0 = n
  let val = eval(ec, n0)
  result = getConstOrdinalValue(val)

proc evalOrdinal*(c: var SemContext, n: Cursor): xint =
  evalOrdinal(addr c, n)

proc getConstStringValue*(val: Cursor): StrId =
  if val.isStringLit:
    result = val.litId
  else:
    result = StrId(0)

proc evalString(c: ptr SemContext, n: Cursor): StrId =
  var ec = initEvalContext(c)
  var n0 = n
  let val = eval(ec, n0)
  result = getConstStringValue(val)

proc evalString*(c: var SemContext, n: Cursor): StrId =
  evalString(addr c, n)

proc annotateOrdinal(buf: var TokenBuf; typ: var Cursor; n: Cursor; err: var bool) =
  var ordinal = getConstOrdinalValue(n)
  if isNaN(ordinal):
    err = true
    return
  let kind = typ.typeKind
  case kind
  of IntT, UIntT, FloatT:
    inc typ
    let bits = typebits(typ.load)
    var suf: string
    var litBuf = createTokenBuf(2)
    case kind
    of IntT:
      suf = "i"
      let val = asSigned(ordinal, err)
      if err: return
      litBuf.addIntLit(val, n.info)
    of UIntT:
      suf = "u"
      let val = asUnsigned(ordinal, err)
      if err: return
      litBuf.addUIntLit(val, n.info)
    of FloatT:
      suf = "f"
      let negative = isNegative(ordinal)
      if negative: negate(ordinal)
      var val = float64(asUnsigned(ordinal, err))
      if err: return
      if negative:
        val = -val
      litBuf.addFloatLit(val, n.info)
    else: bug("unreachable")
    suf.addInt(bits)
    buf.addParLe(SufX, n.info)
    var litCur = beginRead(litBuf)
    while litCur.hasMore:
      buf.addSubtree litCur
      skip litCur
    buf.addStrLit(suf, n.info)
    buf.addParRi()
  of BoolT:
    if n.exprKind in {TrueX, FalseX}:
      buf.addSubtree n
    elif ordinal == zero():
      buf.addParLe(FalseX, n.info)
      buf.addParRi()
    elif ordinal == createXint(1'i64):
      buf.addParLe(TrueX, n.info)
      buf.addParRi()
    else: err = true
  of CharT:
    if n.kind == CharLit:
      buf.addSubtree n
    else:
      let val = asUnsigned(ordinal, err)
      err = err or val < 0 or val > uint64(char.high)
      if not err:
        buf.addCharLit(char(val), n.info)
  of EnumT, HoleyEnumT, AnumT:
    # finds the field sym but could also generate a conversion
    let decl = asEnumDecl(typ)
    var fields = decl.body
    err = true
    fields.into:
      skip fields, SkipType
      if decl.kind == AnumT:
        skip fields, AnyType
      var done = false
      while fields.hasMore and not done:
        let field = takeLocal(fields, SkipFinalParRi)
        var val = field.val
        inc val # skip tuple tag
        let x = getConstOrdinalValue(val)
        if ordinal == x:
          err = false
          buf.addSymUse(field.name.symId, n.info)
          done = true
      while fields.hasMore: skip fields  # mop-up so into closes cleanly
  else:
    err = true

proc findObjectField(objType: Cursor; fieldSym: SymId; typ: var Cursor; exported: var bool): bool =
  ## Walks an object body to find the type and export-status of `fieldSym`.
  ## Returns false if the field is not found. Object fields are nested
  ## inside their owning type and never published as standalone top-level
  ## entries in `prog.mem`, so `tryLoadSym(fieldSym)` cannot resolve them.
  var n = objType
  if n.typeKind != ObjectT: return false
  n = sub(n) # bound the walk to the object body; `n` is a copy
  skip n # parent type
  var iter = initObjFieldIter()
  while nextField(iter, n):
    let r = takeLocal(n, SkipFinalParRi)
    if r.kind in {FldY, GfldY} and r.name.isSymbolDef and r.name.symId == fieldSym:
      typ = r.typ
      exported = not r.exported.isDotToken
      return true
  return false

proc annotateConstantType*(buf: var TokenBuf; typ, n: Cursor) =
  if n.isTagLit and n.tagId == nifstreams.ErrT:
    buf.addSubtree n
    return
  let orig = typ
  var typ = skipModifier(typ)
  var symType = default(Cursor)
  var opened = 0
  while typ.isSymbol:
    let sym = typ.symId
    let res = tryLoadSym(sym)
    if res.status == LacksNothing:
      let decl = asTypeDecl(res.decl)
      if decl.body.typeKind == DistinctT:
        buf.addParLe(DconvX, n.info)
        buf.addSymUse(sym, n.info)
        inc opened
        typ = decl.body
        inc typ # distinct tag
        continue
      else:
        symType = typ
        typ = decl.body
    break

  var err = false
  case n.kind
  of IntLit, UIntLit, CharLit:
    annotateOrdinal(buf, typ, n, err)
  of FloatLit:
    if typ.typeKind == FloatT:
      inc typ
      let bits = typebits(typ.load)
      if bits == 64:
        buf.addSubtree n
      else:
        buf.addParLe(SufX, n.info)
        buf.addSubtree n
        buf.addStrLit("f" & $bits, n.info)
        buf.addParRi()
    else: err = true
  of StrLitKind:
    if not cursorIsNil(symType) and isStringType(symType):
      buf.addSubtree n
    elif typ.typeKind == CstringT:
      buf.addParLe(SufX, n.info)
      buf.addSubtree n
      buf.addStrLit("C", n.info)
      buf.addParRi()
    else: err = true
  of Symbol:
    let res = tryLoadSym(n.symId)
    if res.status == LacksNothing:
      case res.decl.symKind
      of EfldY:
        let field = asLocal(res.decl)
        if field.typ.isSymbol and not cursorIsNil(symType) and
            field.typ.symId == symType.symId:
          # same type as expected
          buf.addSubtree n
        else:
          # might need conversion
          var val = field.val
          inc val # skip tuple tag
          annotateOrdinal(buf, typ, val, err)
      else:
        # other syms are not valid literals
        err = true
    else: err = true
  of OpenTagKind:
    let exprKind = n.exprKind
    case exprKind
    of TrueX, FalseX:
      if typ.typeKind == BoolT:
        buf.addSubtree n
      else:
        # might need conversion
        annotateOrdinal(buf, typ, n, err)
    of SufX:
      var raw = n
      inc raw # skip tag
      annotateConstantType(buf, typ, raw)
    of NilX:
      case typ.typeKind
      of PointerT, PtrT, RefT, CstringT, RoutineTypes, NiltT:
        buf.addSubtree n
      else: err = true
    of AddrX:
      # `addr X` is a valid pointer constant when the target type is a
      # plain pointer. Element-type checking has already happened in
      # `semAddr`; here we only need to validate the constant's shape
      # against the declared pointer kind.
      # `HaddrX` carries a `var T` (MutT) type — not a `ptr T` constant —
      # so it is intentionally not accepted here.
      case typ.typeKind
      of PtrT, PointerT:
        # Special-case `(addr (aconstr (uarray T) e1 ... eN))` (the shape
        # exprexec's ptr-to-nif rule emits): recurse element-wise so any
        # inner OconstrX gets its inline-body type slot replaced with the
        # element type's Symbol. Without this, sem later sees an oconstr
        # with `(object . ...)` as its type slot and rejects with
        # "expected type symbol for object constructor".
        var inner = n
        inc inner # past addr tag
        var isAconstrUarray = false
        if inner.exprKind == AconstrX:
          var t = inner
          inc t # past aconstr tag
          if t.typeKind == UarrayT:
            isAconstrUarray = true
        if isAconstrUarray:
          var aconstr = n
          inc aconstr # past addr tag
          var typSlot = aconstr
          inc typSlot # past aconstr tag → uarray T
          var elemType = typSlot
          inc elemType # past uarray tag → element type
          buf.addParLe(AddrX, n.info)
          buf.addParLe(AconstrX, aconstr.info)
          buf.addSubtree typSlot
          var vals = aconstr
          vals.into:
            skip vals # past uarray type slot
            while vals.hasMore:
              annotateConstantType(buf, elemType, vals)
              skip vals
          buf.addParRi() # close aconstr
          buf.addParRi() # close addr
        else:
          buf.addSubtree n
      else: err = true
    of CastX:
      # `cast[ptr U](addr X)` keeps its cast wrapper through eval; the
      # cast carries the user-declared pointer type that codegen needs.
      # Validate that we're slotting it into a pointer-typed const and
      # pass the whole expression through.
      case typ.typeKind
      of PtrT, PointerT:
        buf.addSubtree n
      else: err = true
    of NanX, InfX, NeginfX:
      if typ.typeKind == FloatT:
        buf.addSubtree n
      else: err = true
    of TupX, TupconstrX:
      if typ.typeKind == TupleT:
        let start = buf.len
        buf.addParLe(TupconstrX, n.info)
        buf.addSubtree typ
        var vals = n
        vals.peekInto:
          if exprKind == TupconstrX:
            skip vals # skip type
          typ.peekInto:
            while vals.hasMore:
              if not typ.hasMore:
                err = true
                break
              annotateConstantType(buf, getTupleFieldType(typ), vals)
              skip typ
              skip vals
            if typ.hasMore: err = true
        if err:
          buf.shrink start
        else:
          buf.addParRi()
      else: err = true
    of BracketX, AconstrX:
      if typ.typeKind == ArrayT: # XXX seq?
        buf.addParLe(AconstrX, n.info)
        buf.addSubtree typ
        var vals = n
        inc typ # tag, get to element type
        vals.peekInto:
          if exprKind == AconstrX:
            skip vals # skip type
          while vals.hasMore:
            annotateConstantType(buf, typ, vals)
            skip vals
        buf.addParRi()
      else: err = true
    of CurlyX, SetconstrX:
      if typ.typeKind == SetT:
        buf.addParLe(SetconstrX, n.info)
        buf.addSubtree typ
        var vals = n
        inc typ # tag, get to element type
        vals.peekInto:
          if exprKind == SetconstrX:
            skip vals # skip type
          while vals.hasMore:
            if vals.substructureKind == RangeU:
              copyInto buf, vals:
                annotateConstantType(buf, typ, vals)
                skip vals
                annotateConstantType(buf, typ, vals)
                skip vals
            else:
              annotateConstantType(buf, typ, vals)
              skip vals
        buf.addParRi()
      else: err = true
    of OconstrX:
      if typ.typeKind == ObjectT and not cursorIsNil(symType):
        # expect object sym type for object constructor.
        # The field's declared type is read from the object body via
        # `findObjectField` because field syms are nested inside their
        # owning type and not loadable through `tryLoadSym`.
        let start = buf.len
        buf.addParLe(OconstrX, n.info)
        buf.addSubtree symType
        var vals = n
        vals.peekInto:
          skip vals # skip type
          while vals.hasMore:
            err = true
            if vals.substructureKind == KvU:
              buf.addParLe(vals.tag, vals.info)
              vals.peekInto:
                if vals.isSymbol:
                  let fieldSym = vals.symId
                  var fieldType = default(Cursor)
                  var fieldExported = false
                  if findObjectField(typ, fieldSym, fieldType, fieldExported):
                    err = false
                    buf.addSubtree vals
                    inc vals
                    annotateConstantType(buf, fieldType, vals)
                    skip vals
                    if vals.hasMore:
                      # optional inheritance
                      takeTree buf, vals
              if not err:
                buf.addParRi()
            if err: break
        if err:
          buf.shrink start
        else:
          buf.addParRi()
      else: err = true
    else:
      # not a literal
      err = true
  else:
    err = true

  if err:
    if opened > 0:
      # could also replace with a general shrink to start
      buf.shrink buf.len - opened
    buf.addParLe nifstreams.ErrT, n.info
    buf.addDotToken()
    let msg = "cannot annotate constant " & asNimCode(n) & " with type " & typeToString(orig)
    buf.addStrLit(msg, n.info)
    buf.addParRi()
  else:
    while opened > 0:
      buf.addParRi()
      dec opened

type
  Bounds* = object
    lo*, hi*: xint

proc enumBounds*(n: Cursor): Bounds =
  assert n.typeKind in {EnumT, HoleyEnumT, AnumT}
  var n = n
  let kind = n.typeKind
  result = Bounds(lo: createNaN(), hi: createNaN())
  n.into:
    skip n # Basetype
    if kind == AnumT:
      skip n # owner object type sym (or dot)
    while n.hasMore:
      let enumField = takeLocal(n, SkipFinalParRi)
      var val = enumField.val
      inc val # skip tuple tag
      let x = evalOrdinal(nil, val)
      if isNaN(result.lo) or x < result.lo: result.lo = x
      if isNaN(result.hi) or x > result.hi: result.hi = x

proc div8Roundup(a: int64): int64 =
  if (a and 7) == 0:
    result = a shr 3
  else:
    result = (a shr 3) + 1

proc bitsetSizeInBytes*(baseType: Cursor): xint =
  var baseType = toTypeImpl baseType
  case baseType.typeKind
  of IntT, UIntT:
    let bits = int pool.integers[baseType.firstSon.intId]
    # - 3 because we do `div 8` as a byte has 8 bits:
    result = createXint(1'i64) shl (bits - 3)
  of CharT:
    result = createXint(256'i64 div 8'i64)
  of BoolT:
    result = createXint(1'i64)
  of EnumT, HoleyEnumT, AnumT:
    let b = enumBounds(baseType)
    # XXX We don't use an offset != 0 anymore for set[MyEnum] construction
    # so we only consider the 'hi' value here:
    var err = false
    let m = asSigned(b.hi, err) + 1'i64
    if err: result = createNaN()
    else:
      result = createXint div8Roundup(m)
      # If the enum has an explicit .size pragma, its base type may be
      # wider than the field values need. Use the base type's byte width
      # as a minimum for set storage.
      var bt = baseType
      inc bt  # skip EnumT/HoleyEnumT/AnumT tag
      if bt.typeKind in {IntT, UIntT}:
        let baseBits = int pool.integers[bt.firstSon.intId]
        let baseBytes = baseBits div 8
        if baseBytes > 0 and result < createXint(baseBytes):
          # IntT with negative field values is the compiler's default
          # for signed enums (not from .size). Only apply when UIntT
          # or when no negative values exist.
          let hasNegativeValues = not isNaN(b.lo) and (b.lo < createXint(0'i64))
          if bt.typeKind == UIntT or not hasNegativeValues:
            result = createXint(baseBytes)
  of RangetypeT:
    var index = baseType
    inc index # tag
    skip index # basetype
    # XXX offset not implemented
    skip index # lo
    let hi = evalOrdinal(nil, index)
    var err = false
    let m = asSigned(hi, err) + 1'i64
    if err: result = createNaN()
    else: result = createXint div8Roundup(m)
  of DistinctT:
    result = bitsetSizeInBytes(baseType.firstSon)
  else:
    result = createNaN()

proc countEnumValues*(n: Cursor): xint =
  result = createNaN()
  if n.isSymbol:
    let sym = tryLoadSym(n.symId)
    if sym.status == LacksNothing:
      var local = asTypeDecl(sym.decl)
      if local.kind == TypeY and local.body.typeKind in {EnumT, HoleyEnumT, AnumT}:
        let b = enumBounds(local.body)
        result = b.hi - b.lo + createXint(1'i64)

proc getArrayIndexLen*(index: Cursor): xint =
  var index = toTypeImpl index
  case index.typeKind
  of EnumT:
    result = countEnumValues(index)
  of IntT, UIntT:
    let bits = int pool.integers[index.firstSon.intId]
    result = createXint(1'i64) shl bits
  of CharT:
    result = createXint 256'i64
  of BoolT:
    result = createXint 2'i64
  of RangetypeT:
    inc index # RangetypeT
    skip index # basetype is irrelevant, we care about the length
    let first = evalOrdinal(nil, index)
    skip index
    let last = evalOrdinal(nil, index)
    result = last - first + createXint(1'i64)
  else:
    result = createNaN()

proc getArrayLen*(n: Cursor): xint =
  # Returns -1 in case of an error.
  assert n.typeKind == ArrayT
  var n = n
  inc n
  skip n # skip basetype
  result = getArrayIndexLen(n)

proc evalBitSetImpl(n, typ: Cursor): seq[uint8] =
  ## returns @[] if it could not be evaluated.
  assert n.exprKind == SetconstrX
  assert typ.typeKind == SetT
  let size = bitsetSizeInBytes(typ.firstSon)
  var err = false
  let s = asSigned(size, err)
  if err:
    return @[]
  result = newSeq[uint8](s)
  var n = n
  n.into:
    skip n # skip set type
    while n.hasMore:
      if n.substructureKind == RangeU:
        var xa = createNaN()
        var xb = createNaN()
        n.into:
          xa = evalOrdinal(nil, n)
          skip n
          xb = evalOrdinal(nil, n)
          skip n
        if not xa.isNaN and not xb.isNaN:
          var i = asUnsigned(xa, err)
          let zb = asUnsigned(xb, err)
          while i <= zb:
            result[i shr 3] = result[i shr 3] or (1'u8 shl (i.uint8 and 7'u8))
            inc i
        else:
          err = true
      else:
        let xa = evalOrdinal(nil, n)
        skip n
        if not xa.isNaN:
          let i = asUnsigned(xa, err)
          result[i shr 3] = result[i shr 3] or (1'u8 shl (i.uint8 and 7'u8))
        else:
          err = true
  if err:
    return @[]

proc evalBitSet*(n, typ: Cursor): seq[uint8] = evalBitSetImpl(n, typ)
