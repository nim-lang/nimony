#
#
#           Nimony
#        (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.
#

## Helpers for matching value (`static`) generic parameters and the compile-time
## values bound to them. These are the parts of the static-parameter machinery
## that do *not* depend on the `Match` object; the `Match`-bound logic
## (`foldValueExpr`, `staticValueToBind`, `bindStaticTypevar`, ...) stays in
## `sigmatch`.

import std/assertions
include ".." / lib / nifprelude
import nimony_model, decls, programs, builtintypes
import ".." / lib / symparser
import ".." / models / tags

proc isStaticTypevar*(s: SymId): bool =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  result = res.decl.symKind == StaticTypevarY

proc sameStaticSymbol*(a, b: SymId): bool =
  if a == b:
    return true
  let sa = pool.syms[a]
  let sb = pool.syms[b]
  result = isInstantiation(sa) and isInstantiation(sb) and
    removeModule(sa) == removeModule(sb)

proc isStaticValue*(n: Cursor): bool =
  ## A canonical compile-time value as bound to a `staticTypevar`: a primitive
  ## literal, a const/enum-field/value-typevar symbol, or a typed aggregate
  ## constructor (array/set/tuple/object) whose elements are themselves static.
  if not n.hasMore: return false
  case n.kind
  of IntLit, UIntLit, FloatLit, CharLit, StrLit:
    result = true
  of Symbol:
    let res = tryLoadSym(n.symId)
    result = res.status == LacksNothing and
      res.decl.symKind in {ConstY, EfldY, StaticTypevarY}
  of TagLit:
    case n.exprKind
    of FalseX, TrueX:
      result = true
    of SufX:
      var elem = n
      inc elem
      result = isStaticValue(elem)
    of AconstrX, SetconstrX, TupconstrX, OconstrX:
      var elem = n
      elem = sub(elem)
      skip elem # type
      result = true
      while elem.hasMore:
        if elem.substructureKind in {KvU, RangeU}:
          elem = sub(elem)
          skip elem # key or range start
          if not isStaticValue(elem):
            result = false
            break
          skip elem
          if not elem.hasMore:
            break
          inc elem
        else:
          if not isStaticValue(elem):
            result = false
            break
          skip elem
    else:
      result = false
  else:
    result = false

proc staticValueType*(a: Cursor): Cursor =
  ## The type of a static value: a symbol's declared type or an aggregate
  ## constructor's leading type node.
  result = default(Cursor)
  if not a.hasMore: return
  case a.kind
  of Symbol:
    let res = tryLoadSym(a.symId)
    if res.status == LacksNothing and isLocal(res.decl.symKind):
      result = asLocal(res.decl).typ
  of TagLit:
    case a.exprKind
    of AconstrX, SetconstrX, TupconstrX, OconstrX:
      var typ = a
      inc typ
      result = typ
    else:
      discard
  else:
    discard

proc staticOpenArrayElemType*(t: Cursor): Cursor =
  ## If `t` (following type aliases) is an `openArray[E]` or `varargs[E]`,
  ## its element type `E`; otherwise nil.
  result = default(Cursor)
  var t = t
  var depth = 0
  while t.isSymbol and depth < 20:
    let res = tryLoadSym(t.symId)
    if res.status != LacksNothing or res.decl.symKind != TypeY:
      return default(Cursor)
    let decl = asTypeDecl(res.decl)
    if decl.typevars.typeKind == InvokeT:
      t = decl.typevars
      break
    elif decl.body.isSymbol:
      t = decl.body
    else:
      return default(Cursor)
    inc depth
  if t.typeKind == InvokeT:
    inc t
    if t.isSymbol and pool.syms[t.symId] == OpenArrayHeadName:
      inc t
      result = t
  elif t.typeKind == VarargsT:
    t = sub(t)
    if t.hasMore:
      result = t

proc staticValueTypeMatches*(elemType, valueType: Cursor): bool =
  ## Whether a static value of type `valueType` may bind a value parameter whose
  ## declared element type is `elemType`, including `static[openArray[T]]`/
  ## `varargs[T]` satisfied by an `array[N, T]` constructor.
  if cursorIsNil(valueType):
    return false
  if sameTrees(elemType, valueType):
    return true
  if elemType.isSymbol and valueType.isSymbol:
    return sameStaticSymbol(elemType.symId, valueType.symId)
  if elemType.typeKind == VarargsT and valueType.typeKind == ArrayT:
    var elem = elemType
    elem = sub(elem)
    if not elem.hasMore:
      return true
    var arrElem = valueType
    inc arrElem
    return sameTrees(elem, arrElem)
  let openArrayElem = staticOpenArrayElemType(elemType)
  if not cursorIsNil(openArrayElem) and valueType.typeKind == ArrayT:
    var arrElem = valueType
    inc arrElem
    return sameTrees(openArrayElem, arrElem)
  false
