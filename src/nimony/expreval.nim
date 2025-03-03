#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## expression evaluator for simple constant expressions, not meant to be complete

import std / assertions

include nifprelude
import nimony_model, decls, programs, xints, semdata, renderer

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

proc eval*(c: var EvalContext; n: var Cursor): Cursor =
  template error(msg: string; info: PackedLineInfo) =
    result = c.error(msg, info)
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
        return local.val
      of EfldY:
        inc local.val # takes the first counter field
        return local.val
      else: discard
    error "cannot evaluate symbol at compile time: " & pool.syms[symId], info
  of StringLit, CharLit, IntLit, UIntLit, FloatLit:
    result = n
    inc n
  of ParLe:
    case n.exprKind
    of TrueX, FalseX:
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
    of ConvX:
      let nOrig = n
      inc n
      let typ = n
      skip n
      let val = propagateError eval(c, n)
      skipParRi n
      if typ.typeKind == CstringT and val.kind == StringLit:
        result = val
      else:
        # other conversions not implemented
        error "cannot evaluate expression at compile time: " & asNimCode(nOrig), nOrig.info
    of DconvX:
      inc n # tag
      skip n # type
      result = eval(c, n)
      skipParRi n
    of IsMainModuleX:
      inc n
      skipParRi n
      if c.c == nil:
        error "cannot evaluate expression at compile time: " & asNimCode(n), n.info
      elif IsMain in c.c.moduleFlags:
        result = c.getTrueValue()
      else:
        result = c.getFalseValue()
    else:
      if n.tagId == ErrT:
        result = n
        skip n
      else:
        error "cannot evaluate expression at compile time: " & asNimCode(n), n.info
  else:
    error "cannot evaluate expression at compile time: " & asNimCode(n), n.info

proc evalExpr*(c: var SemContext, n: var Cursor): TokenBuf =
  var ec = initEvalContext(addr c)
  let val = eval(ec, n)
  result = createTokenBuf(val.span)
  result.addSubtree val

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

proc countEnumValues*(n: Cursor): xint =
  result = createNaN()
  if n.kind == Symbol:
    let sym = tryLoadSym(n.symId)
    if sym.status == LacksNothing:
      var local = asTypeDecl(sym.decl)
      if local.kind == TypeY and local.body.typeKind in {EnumT, HoleyEnumT}:
        let b = enumBounds(local.body)
        result = b.hi - b.lo + createXint(1'i64)

proc div8Roundup(a: int64): int64 =
  if (a and 7) == 0:
    result = a shr 3
  else:
    result = (a shr 3) + 1

proc toTypeImpl*(n: Cursor): Cursor =
  result = n
  var counter = 20
  while counter > 0 and result.kind == Symbol:
    dec counter
    let sym = tryLoadSym(result.symId)
    if sym.status == LacksNothing:
      var local = asTypeDecl(sym.decl)
      if local.kind == TypeY:
        result = local.body
    else:
      raiseAssert "could not load: " & pool.syms[result.symId]

proc bitsetSizeInBytes*(baseType: Cursor): xint =
  var baseType = toTypeImpl baseType
  case baseType.typeKind
  of IntT, UIntT:
    let bits = pool.integers[baseType.firstSon.intId]
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

proc getArrayIndexLen*(index: Cursor): xint =
  var index = toTypeImpl index
  case index.typeKind
  of EnumT:
    result = countEnumValues(index)
  of IntT, UIntT:
    let bits = pool.integers[index.firstSon.intId]
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
