#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## expression evaluator for simple constant expressions, not meant to be complete

import std / assertions

include ".." / lib / nifprelude
import nimony_model, decls, programs, xints, semdata, renderer, builtintypes, typeprops

type
  EvalContext* = object
    c: ptr SemContext
    values: seq[TokenBuf]
    trueValue, falseValue: Cursor

proc isConstBoolValue*(n: Cursor): bool =
  n.exprKind in {TrueX, FalseX}

proc isConstIntValue*(n: Cursor): bool =
  n.kind == IntLit

proc isConstUIntValue*(n: Cursor): bool =
  n.kind == UIntLit

proc isConstStringValue*(n: Cursor): bool =
  n.kind == StringLit

proc isConstCharValue*(n: Cursor): bool =
  n.kind == CharLit

proc initEvalContext*(c: ptr SemContext): EvalContext =
  result = EvalContext(c: c, values: @[])

proc skipParRi(n: var Cursor) =
  if n.kind == ParRi:
    inc n
  else:
    error "expected ')', but got: ", n

proc error(c: var EvalContext, msg: string, info: PackedLineInfo): Cursor =
  let i = c.values.len
  c.values.add createTokenBuf(4)
  c.values[i].addParLe ErrT, info
  c.values[i].addDotToken()
  c.values[i].addStrLit msg
  c.values[i].addParRi()
  result = cursorAt(c.values[i], 0)

proc getTrueValue(c: var EvalContext): Cursor =
  if c.trueValue == default(Cursor):
    let i = c.values.len
    c.values.add createTokenBuf(2)
    c.values[i].addParLe(TrueX, NoLineInfo)
    c.values[i].addParRi()
    c.trueValue = cursorAt(c.values[i], 0)
  result = c.trueValue

proc getFalseValue(c: var EvalContext): Cursor =
  if c.falseValue == default(Cursor):
    let i = c.values.len
    c.values.add createTokenBuf(2)
    c.values[i].addParLe(FalseX, NoLineInfo)
    c.values[i].addParRi()
    c.falseValue = cursorAt(c.values[i], 0)
  result = c.falseValue

proc getConstOrdinalValue*(val: Cursor): xint =
  case val.kind
  of CharLit:
    result = createXint val.uoperand
  of IntLit:
    result = createXint pool.integers[val.intId]
  of UIntLit:
    result = createXint pool.uintegers[val.uintId]
  of ParLe:
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
  let i = c.values.len
  c.values.add createTokenBuf(1)
  c.values[i].add tok
  result = cursorAt(c.values[i], 0)

proc stringValue(c: var EvalContext; s: string; info: PackedLineInfo): Cursor {.inline.} =
  result = singleToken(c, strToken(pool.strings.getOrIncl(s), info))

proc intValue(c: var EvalContext; i: int64; info: PackedLineInfo): Cursor {.inline.} =
  result = singleToken(c, intToken(pool.integers.getOrIncl(i), info))

proc uintValue(c: var EvalContext; u: uint64; info: PackedLineInfo): Cursor {.inline.} =
  result = singleToken(c, uintToken(pool.uintegers.getOrIncl(u), info))

proc floatValue(c: var EvalContext; f: float; info: PackedLineInfo): Cursor {.inline.} =
  result = singleToken(c, floatToken(pool.floats.getOrIncl(f), info))

proc charValue(c: var EvalContext; ch: char; info: PackedLineInfo): Cursor {.inline.} =
  result = singleToken(c, charToken(ch, info))

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

proc evalCall(c: var EvalContext; n: Cursor): Cursor =
  var callee = n
  inc callee
  if callee.kind != Symbol:
    cannotEval(n)
    return
  let res = tryLoadSym(callee.symId)
  if res.status != LacksNothing or not isRoutine(res.decl.symKind):
    cannotEval(n)
    return
  let routine = asRoutine(res.decl)
  var op = ""
  var pragmas = routine.pragmas
  if pragmas.substructureKind == PragmasU:
    inc pragmas
    while pragmas.kind != ParRi:
      var prag = pragmas
      if prag.pragmaKind == SemanticsP:
        inc prag
        if prag.kind in {Ident, StringLit}:
          op = pool.strings[prag.litId]
          break
      skip pragmas
  var args = n
  inc args
  skip args
  case op
  of "string.&":
    let a = eval(c, args)
    let b = eval(c, args)
    if a.kind != StringLit or b.kind != StringLit or args.kind != ParRi:
      cannotEval(n)
      return
    let val = pool.strings[a.litId] & pool.strings[b.litId]
    result = stringValue(c, val, n.info)
  of "string.==":
    let a = eval(c, args)
    let b = eval(c, args)
    if a.kind != StringLit or b.kind != StringLit or args.kind != ParRi:
      cannotEval(n)
      return
    let val = pool.strings[a.litId] == pool.strings[b.litId]
    result = boolValue(c, val)
  of "string.len":
    let a = eval(c, args)
    if a.kind != StringLit or args.kind != ParRi:
      cannotEval(n)
      return
    let val = pool.strings[a.litId].len
    result = intValue(c, val, n.info)
  else:
    var evaluatedCall = createTokenBuf(16)
    evaluatedCall.addParLe CallS, n.info
    evaluatedCall.addSymUse routine.name.symId, n.info
    while args.kind != ParRi:
      let thisArg = args
      let x = eval(c, args)
      if x.kind == ParLe and x.tagId == nifstreams.ErrT:
        cannotEval(thisArg)
        return
      evaluatedCall.addSubtree x
    evaluatedCall.addParRi()

    let i = c.values.len
    c.values.add createTokenBuf(12)
    assert c.c.executeCall != nil
    let errorMsg = c.c.executeCall(c.c[], routine, c.values[i], cursorAt(evaluatedCall, 0), n.info)
    if errorMsg.len == 0:
      result = cursorAt(c.values[i], 0)
    else:
      result = c.error("cannot evaluate expression at compile time: " & asNimCode(n) & "\n\n" & errorMsg, n.info)

template evalOrdBinOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  let orig = n
  inc n # tag
  let isSigned = n.typeKind == IntT
  skip n # type
  let a = getConstOrdinalValue propagateError eval(c, n)
  let b = getConstOrdinalValue propagateError eval(c, n)
  skipParRi n
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
  inc n # tag
  skip n # type
  let a = propagateError eval(c, n)
  let b = propagateError eval(c, n)
  skipParRi n
  if a.kind == FloatLit and b.kind == FloatLit:
    let rf = opr(pool.floats[a.floatId], pool.floats[b.floatId])
    result = floatValue(c, rf, orig.info)
  else:
    cannotEval orig

template evalCmpOp(c: var EvalContext; n: var Cursor; opr: untyped) {.dirty.} =
  let orig = n
  inc n # tag
  let t = n
  skip n # type
  if t.typeKind == FloatT:
    let a = propagateError eval(c, n)
    let b = propagateError eval(c, n)
    skipParRi n
    if a.kind == FloatLit and b.kind == FloatLit:
      let rf = opr(pool.floats[a.floatId], pool.floats[b.floatId])
      result = boolValue(c, rf)
    else:
      cannotEval orig
  else:
    let a = getConstOrdinalValue propagateError eval(c, n)
    let b = getConstOrdinalValue propagateError eval(c, n)
    skipParRi n
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
  inc n # tag
  let isSigned = n.typeKind == IntT
  skip n # type
  let a = getConstOrdinalValue propagateError eval(c, n)
  skipParRi n
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
  inc n # tag
  skip n # type
  let a = propagateError eval(c, n)
  skipParRi n
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
  inc n # tag
  let isSigned = n.typeKind == IntT
  var bits = -1
  case n.typeKind
  of IntT, UintT:
    inc n
    bits = typebits(n.load)
    skipToEnd n
  else:
    error "expected int or uint type for shift operation, got: " & typeToString(n), n.info
  if bits < 0: bits = c0.c.g.config.bits
  let a = getConstOrdinalValue propagateError eval(c0, n)
  let b = getConstOrdinalValue propagateError eval(c0, n)
  skipParRi n
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
  inc n # tag
  let isSigned = n.typeKind == IntT
  var bits = -1
  case n.typeKind
  of IntT, UintT:
    inc n
    bits = typebits(n.load)
    skipToEnd n
  else:
    error "expected int or uint type for shl, got: " & typeToString(n), n.info
  if bits < 0: bits = c.c.g.config.bits
  let a = getConstOrdinalValue propagateError eval(c, n)
  skipParRi n
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
    result.add charToken(char x, NoLineInfo)
  else:
    var hasError = true
    if typ.kind == Symbol:
      let sym = tryLoadSym(typ.symId)
      if sym.status == LacksNothing:
        var local = asTypeDecl(sym.decl)
        if local.kind == TypeY and local.body.typeKind in {EnumT, HoleyEnumT}:
          hasError = false
          result.addIntLit x
    if hasError:
      assert false, "Got unexpected type: " & toString(typ)

proc bitSetToTokens(result: var TokenBuf; x: seq[uint8]; elementTyp: Cursor; info: PackedLineInfo) =
  result.addParLe SetConstrX, info
  result.buildTree TagId(SetT), NoLineInfo:
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

proc evalBitSet*(n, typ: Cursor): seq[uint8]

proc evalOrdinal(c: ptr SemContext, n: Cursor): xint

proc evalInSet(c: var EvalContext; n: var Cursor): Cursor =
  inc n # tag
  assert n.typeKind == SetT
  skip n # skip type
  var a = eval(c, n)
  var b = evalOrdinal(nil, n)
  skip n # skips b
  skipParRi n # skip last parRi
  assert a.exprKind == SetConstrX, "got " & toString(a)
  inc a # skip set tag
  skip a # skip set type

  var isInSet = false
  while a.kind != ParRi:
    if a.substructureKind == RangeU:
      inc a
      let xa = evalOrdinal(nil, a)
      skip a
      let xb = evalOrdinal(nil, a)
      skip a
      if b >= xa and b <= xb:
        isInSet = true
        break
      skipParRi(a)
    else:
      let xa = evalOrdinal(nil, a)
      if xa == b:
        isInSet = true
        break
      skip a

  result = boolValue(c, isInSet)

# Number of set bits for all values of int8
const populationCount: array[uint8, uint8] = block:
    var arr: array[uint8, uint8]

    proc countSetBits(x: uint8): uint8 =
      return
        ( x and 0b00000001'u8) +
        ((x and 0b00000010'u8) shr 1) +
        ((x and 0b00000100'u8) shr 2) +
        ((x and 0b00001000'u8) shr 3) +
        ((x and 0b00010000'u8) shr 4) +
        ((x and 0b00100000'u8) shr 5) +
        ((x and 0b01000000'u8) shr 6) +
        ((x and 0b10000000'u8) shr 7)


    for it in low(uint8)..high(uint8):
      arr[it] = countSetBits(cast[uint8](it))

    arr

proc bitSetCard(x: seq[uint8]): BiggestInt =
  result = 0
  for it in x:
    result.inc int(populationCount[it])

proc evalCardSet(c: var EvalContext; n: var Cursor): Cursor =
  let info = n.info
  inc n # tag
  assert n.typeKind == SetT
  skip n # skip type
  var a = eval(c, n)
  skipParRi n # skip last parRi

  assert a.exprKind == SetConstrX, "got " & toString(a)
  var typeA = a
  inc typeA

  let setA = evalBitSet(a, typeA)
  result = intValue(c, bitSetCard(setA), info)

proc evalSetOp(c: var EvalContext; n: var Cursor; op: ExprKind): Cursor =
  let info = n.info
  inc n # tag
  assert n.typeKind == SetT
  var elementTyp = n
  inc elementTyp
  skip n # skip type
  var a = eval(c, n)
  var b = eval(c, n)
  skipParRi n # skip last parRi
  assert a.exprKind == SetConstrX, "got " & toString(a)
  assert b.exprKind == SetConstrX, "got " & toString(b)
  var typeA = a
  inc typeA
  var typeB = b
  inc typeB
  assert sameTrees(typeA, typeB)  # must be the same type
  let setA = evalBitSet(a, typeA)
  let setB = evalBitSet(b, typeB)
  assert setA.len == setB.len
  var setRes = newSeq[uint8](setA.len)
  case op
  of PlusSetX:
    for i in 0 ..< setA.len:
      setRes[i] = setA[i] or setB[i]
  of MinusSetX:
    for i in 0 ..< setA.len:
      setRes[i] = setA[i] and not setB[i]
  of XorSetX:
    for i in 0 ..< setA.len:
      setRes[i] = setA[i] xor setB[i]
  of MulSetX:
    for i in 0 ..< setA.len:
      setRes[i] = setA[i] and setB[i]
  else:
    assert false, "unexpected operation: " & $op

  let valPos = c.values.len
  c.values.add createTokenBuf()
  c.values[valPos].bitSetToTokens(setRes, elementTyp, info)
  result = cursorAt(c.values[valPos], 0)

proc eval*(c: var EvalContext; n: var Cursor): Cursor =
  template propagateError(r: Cursor): Cursor =
    let val = r
    if val.kind == ParLe and val.tagId == nifstreams.ErrT:
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
    inc n
    let sym = tryLoadSym(symId)
    if sym.status == LacksNothing:
      var local = asLocal(sym.decl)
      case local.kind
      of ConstY:
        return eval(c, local.val)
      of EfldY:
        inc local.val # takes the first counter field
        return eval(c, local.val)
      else: discard
    error "cannot evaluate symbol at compile time: " & pool.syms[symId], info
  of StringLit, CharLit, IntLit, UIntLit, FloatLit:
    result = n
    inc n
  of ParLe:
    let exprKind = n.exprKind
    case exprKind
    of TrueX, FalseX, NanX, InfX, NeginfX, NilX:
      result = n
      skip n
    of AndX:
      inc n
      let a = propagateError eval(c, n)
      if a.exprKind == FalseX:
        skipToEnd n
        return a
      elif a.exprKind != TrueX:
        error "expected bool for operand of `and` but got: " & asNimCode(a), n.info
        return
      let b = propagateError eval(c, n)
      if not isConstBoolValue(b):
        error "expected bool for operand of `and` but got: " & asNimCode(b), n.info
        return
      else:
        skipParRi n
        return b
    of OrX:
      inc n
      let a = propagateError eval(c, n)
      if a.exprKind == TrueX:
        skipToEnd n
        return a
      elif a.exprKind != FalseX:
        error "expected bool for operand of `or` but got: " & asNimCode(a), n.info
        return
      let b = propagateError eval(c, n)
      if not isConstBoolValue(b):
        error "expected bool for operand of `or` but got: " & asNimCode(b), n.info
        return
      else:
        skipParRi n
        return b
    of NotX:
      inc n
      let a = propagateError eval(c, n)
      if a.exprKind == TrueX:
        skipParRi n
        return c.getFalseValue()
      elif a.exprKind == FalseX:
        skipParRi n
        return c.getTrueValue()
      else:
        error "expected bool for operand of `not` but got: " & asNimCode(a), n.info
        return
    of SufX:
      # we only need raw value
      inc n
      result = n
      skipToEnd n
    of ConvX, HconvX:
      let nOrig = n
      inc n
      var isDistinct = false
      var typ = skipDistinct(n, isDistinct)
      skip n
      let val = propagateError eval(c, n)
      skipParRi n
      if typ.typeKind == CstringT and val.kind == StringLit:
        result = val
      elif typ.typeKind == FloatT:
        if val.kind == FloatLit:
          result = val
        else:
          # treats it as an ordinal value
          let x = getConstOrdinalValue(val)
          let f = toFloat64(x)
          result = floatValue(c, f, nOrig.info)
      elif typ.typeKind == UIntT:
        let x = getConstOrdinalValue(val)
        var err = false
        let u = asUnsigned(x, err)
        if err:
          cannotEval nOrig
        else:
          result = uintValue(c, u, nOrig.info)
      elif typ.typeKind == IntT:
        let x = getConstOrdinalValue(val)
        var err = false
        let i = asSigned(x, err)
        if err:
          cannotEval nOrig
        else:
          result = intValue(c, i, nOrig.info)
      elif typ.typeKind == CharT:
        let x = getConstOrdinalValue(val)
        var err = false
        let ch = asUnsigned(x, err)
        if err or ch >= 256u:
          cannotEval nOrig
        else:
          result = charValue(c, char(ch), nOrig.info)
      else:
        # other conversions not implemented
        cannotEval nOrig
    of DconvX:
      inc n # tag
      skip n # type
      result = eval(c, n)
      skipParRi n
    of ExprX:
      let orig = n
      inc n # tag
      result = eval(c, n)
      if n.kind == ParRi:
        inc n
      else:
        # was not a trivial ExprX, so we could not evaluate it
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
    of IsMainModuleX:
      inc n
      skipParRi n
      if c.c == nil:
        cannotEval n
      else:
        let val = IsMain in c.c.moduleFlags
        result = boolValue(c, val)
    of AconstrX, SetconstrX, TupconstrX,
        BracketX, CurlyX, TupX:
      let valPos = c.values.len
      c.values.add createTokenBuf(16)
      c.values[valPos].add n
      inc n
      if exprKind in {AconstrX, SetconstrX, TupconstrX}:
        # add type
        takeTree c.values[valPos], n
      while n.kind != ParRi:
        if exprKind == SetConstrX and n.substructureKind == RangeU:
          c.values[valPos].takeToken n
          var a = propagateError eval(c, n)
          c.values[valPos].addSubtree a
          var b = propagateError eval(c, n)
          c.values[valPos].addSubtree b
          c.values[valPos].takeToken n
        elif exprKind == TupconstrX:
          let isKv = n.substructureKind == KvU
          if isKv:
            inc n # tag
            skip n # key
          let elem = propagateError eval(c, n)
          c.values[valPos].addSubtree elem
          if isKv:
            inc n
        else:
          let elem = propagateError eval(c, n)
          c.values[valPos].addSubtree elem
      takeParRi c.values[valPos], n
      result = cursorAt(c.values[valPos], 0)
    of CallKinds:
      result = evalCall(c, n)
      skip n
    of SizeofX:
      if c.c.g.config.compat:
        var orig = n
        inc n
        let s = c.c.semGetSize(c.c[], n)
        var err = false
        let value = asSigned(s, err)
        if err:
          cannotEval orig
        else:
          result = intValue(c, value, orig.info)
      else:
        cannotEval n
    of PlusSetX, MinusSetX, XorSetX, MulSetX:
      result = evalSetOp(c, n, n.exprKind)
    of InSetX:
      result = evalInSet(c, n)
    of CardX:
      result = evalCardSet(c, n)
    else:
      if n.tagId == ErrT:
        result = n
        skip n
      else:
        cannotEval n
  else:
    cannotEval n

proc evalExpr*(c: var SemContext, n: var Cursor): TokenBuf =
  var ec = initEvalContext(addr c)
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
  if val.kind == StringLit:
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
    if bits < 0 and
        ((kind == IntT and n.kind == IntLit) or
          (kind == UIntT and n.kind == UIntLit)):
      buf.add n
    else:
      var tok: PackedToken
      var suf: string
      case kind
      of IntT:
        suf = "i"
        let val = asSigned(ordinal, err)
        if err: return
        tok = intToken(pool.integers.getOrIncl(val), n.info)
      of UIntT:
        suf = "u"
        let val = asUnsigned(ordinal, err)
        if err: return
        tok = uintToken(pool.uintegers.getOrIncl(val), n.info)
      of FloatT:
        suf = "f"
        let negative = isNegative(ordinal)
        if negative: negate(ordinal)
        var val = float64(asUnsigned(ordinal, err))
        if err: return
        if negative:
          val = -val
        tok = floatToken(pool.floats.getOrIncl(val), n.info)
      else: bug("unreachable")
      if bits >= 0:
        suf.addInt(bits)
        buf.add parLeToken(SufX, n.info)
        buf.add tok
        buf.add strToken(pool.strings.getOrIncl(suf), n.info)
        buf.addParRi()
      else:
        buf.add tok
  of BoolT:
    if n.exprKind in {TrueX, FalseX}:
      buf.addSubtree n
    elif ordinal == zero():
      buf.add parLeToken(FalseX, n.info)
      buf.addParRi()
    elif ordinal == createXint(1'i64):
      buf.add parLeToken(TrueX, n.info)
      buf.addParRi()
    else: err = true
  of CharT:
    if n.kind == CharLit:
      buf.add n
    else:
      let val = asUnsigned(ordinal, err)
      err = err or val < 0 or val > uint64(char.high)
      if not err:
        buf.add charToken(char(val), n.info)
  of EnumT, HoleyEnumT:
    # finds the field sym but could also generate a conversion
    let decl = asEnumDecl(typ)
    var fields = decl.firstField
    err = true
    while fields.kind != ParRi:
      let field = takeLocal(fields, SkipFinalParRi)
      var val = field.val
      inc val # skip tuple tag
      let x = getConstOrdinalValue(val)
      if ordinal == x:
        err = false
        buf.add symToken(field.name.symId, n.info)
        break
  else:
    err = true

proc annotateConstantType*(buf: var TokenBuf; typ, n: Cursor) =
  if n.kind == ParLe and n.tagId == ErrT:
    buf.addSubtree n
    return
  let orig = typ
  var typ = skipModifier(typ)
  var symType = default(Cursor)
  var opened = 0
  while typ.kind == Symbol:
    let sym = typ.symId
    let res = tryLoadSym(sym)
    if res.status == LacksNothing:
      let decl = asTypeDecl(res.decl)
      if decl.body.typeKind == DistinctT:
        buf.add parLeToken(DconvX, n.info)
        buf.add symToken(sym, n.info)
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
      if bits < 0 or bits == 64:
        buf.add n
      else:
        buf.add parLeToken(SufX, n.info)
        buf.add n
        buf.add strToken(pool.strings.getOrIncl("f" & $bits), n.info)
        buf.addParRi()
    else: err = true
  of StringLit:
    if not cursorIsNil(symType) and isStringType(symType):
      buf.add n
    elif typ.typeKind == CstringT:
      buf.add parLeToken(SufX, n.info)
      buf.add n
      buf.add strToken(pool.strings.getOrIncl("C"), n.info)
      buf.addParRi()
    else: err = true
  of Symbol:
    let res = tryLoadSym(n.symId)
    if res.status == LacksNothing:
      case res.decl.symKind
      of EfldY:
        let field = asLocal(res.decl)
        if field.typ.kind == Symbol and not cursorIsNil(symType) and
            field.typ.symId == symType.symId:
          # same type as expected
          buf.add n
        else:
          # might need conversion
          var val = field.val
          inc val # skip tuple tag
          annotateOrdinal(buf, typ, val, err)
      else:
        # other syms are not valid literals
        err = true
    else: err = true
  of ParLe:
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
    of NanX, InfX, NeginfX:
      if typ.typeKind == FloatT:
        buf.addSubtree n
      else: err = true
    of TupX, TupconstrX:
      if typ.typeKind == TupleT:
        let start = buf.len
        buf.add parLeToken(TupconstrX, n.info)
        buf.addSubtree typ
        var vals = n
        inc vals
        if exprKind == TupconstrX:
          skip vals # skip type
        inc typ # tag
        while vals.kind != ParRi:
          if typ.kind == ParRi:
            err = true
            break
          annotateConstantType(buf, getTupleFieldType(typ), vals)
          skip typ
          skip vals
        if typ.kind != ParRi: err = true
        if err:
          buf.shrink start
        else:
          buf.addParRi()
      else: err = true
    of BracketX, AconstrX:
      if typ.typeKind == ArrayT: # XXX seq?
        buf.add parLeToken(AconstrX, n.info)
        buf.addSubtree typ
        var vals = n
        inc vals
        if exprKind == AconstrX:
          skip vals # skip type
        inc typ # tag, get to element type
        while vals.kind != ParRi:
          annotateConstantType(buf, typ, vals)
          skip vals
        buf.addParRi()
      else: err = true
    of CurlyX, SetconstrX:
      if typ.typeKind == SetT:
        buf.add parLeToken(SetconstrX, n.info)
        buf.addSubtree typ
        var vals = n
        inc vals
        if exprKind == SetconstrX:
          skip vals # skip type
        inc typ # tag, get to element type
        while vals.kind != ParRi:
          if vals.substructureKind == RangeU:
            buf.add vals
            inc vals
            annotateConstantType(buf, typ, vals)
            skip vals
            annotateConstantType(buf, typ, vals)
            skip vals
            takeParRi buf, vals
          else:
            annotateConstantType(buf, typ, vals)
            skip vals
        buf.addParRi()
      else: err = true
    of OconstrX:
      if typ.typeKind == ObjectT and not cursorIsNil(symType):
        # expect object sym type for object constructor
        let start = buf.len
        buf.add parLeToken(OconstrX, n.info)
        buf.addSubtree symType
        var vals = n
        inc vals
        skip vals # skip type
        inc typ # tag
        while vals.kind != ParRi:
          err = true
          if vals.substructureKind == KvU:
            buf.add vals
            inc vals
            if vals.kind == Symbol:
              let fieldSym = vals.symId
              let res = tryLoadSym(fieldSym)
              if res.status == LacksNothing and res.decl.symKind == FldY:
                err = false
                buf.add vals
                inc vals
                annotateConstantType(buf, asLocal(res.decl).typ, vals)
                skip vals
                if vals.kind != ParRi:
                  # optional inheritance
                  takeTree buf, vals
                takeParRi buf, vals
          if err: break
        if typ.kind != ParRi: err = true
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
    buf.addParLe ErrT, n.info
    buf.addDotToken()
    let msg = "cannot annotate constant " & asNimCode(n) & " with type " & typeToString(orig)
    buf.add strToken(pool.strings.getOrIncl(msg), n.info)
    buf.addParRi()
  else:
    while opened > 0:
      buf.addParRi()
      dec opened

type
  Bounds* = object
    lo*, hi*: xint

proc enumBounds*(n: Cursor): Bounds =
  assert n.typeKind in {EnumT, HoleyEnumT}
  var n = n
  inc n # EnumT
  skip n # Basetype
  result = Bounds(lo: createNan(), hi: createNaN())
  while n.kind != ParRi:
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
  of EnumT, HoleyEnumT:
    let b = enumBounds(baseType)
    # XXX We don't use an offset != 0 anymore for set[MyEnum] construction
    # so we only consider the 'hi' value here:
    var err = false
    let m = asSigned(b.hi, err) + 1'i64
    if err: result = createNaN()
    else: result = createXint div8Roundup(m)
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
  if n.kind == Symbol:
    let sym = tryLoadSym(n.symId)
    if sym.status == LacksNothing:
      var local = asTypeDecl(sym.decl)
      if local.kind == TypeY and local.body.typeKind in {EnumT, HoleyEnumT}:
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

proc evalBitSet*(n, typ: Cursor): seq[uint8] =
  ## returns @[] if it could not be evaluated.
  assert n.exprKind == SetConstrX
  assert typ.typeKind == SetT
  let size = bitsetSizeInBytes(typ.firstSon)
  var err = false
  let s = asSigned(size, err)
  if err:
    return @[]
  result = newSeq[uint8](s)
  var n = n
  inc n # skip set tag
  skip n # skip set type
  while n.kind != ParRi:
    if n.substructureKind == RangeU:
      inc n
      let xa = evalOrdinal(nil, n)
      skip n
      let xb = evalOrdinal(nil, n)
      skip n
      if n.kind == ParRi: inc n
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
