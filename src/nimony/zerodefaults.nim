#
#
#           Nimony Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Is a type's default value all zero bits? Sem asks it to write
## `default(array[I, T])` as the all-zero `(aconstr array[I, T])` rather than
## a call (`semantics: "defaultArray"`), the hexer to write an array default
## the same way (`defaultvalues.addDefaultValue`).

import std / assertions
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import nimony_model, decls, programs, typeprops, expreval, xints

proc isZeroDefault*(typ: Cursor): bool =
  ## Is `default(typ)` — what `addDefaultValue` builds — all zero bits?
  ## Conservative: `false` means "not known to be", never "known not to be".
  ## A field with a declared default counts as non-zero whatever its value,
  ## and a case object's branches are all checked, not just the first one.
  let impl = toTypeImpl(typ)
  case impl.typeKind
  of IntT, UIntT, FloatT, CharT, BoolT, SetT,
     PtrT, RefT, RoutineTypes, CstringT, PointerT, NiltT, OutT:
    result = true
  of EnumT, HoleyEnumT, AnumT:
    var err = false
    result = asSigned(enumBounds(impl).lo, err) == 0 and not err
  of MutT, LentT:
    let base = impl.childCursor
    result = not isViewType(base) or isZeroDefault(base)
  of ObjectT:
    if typ.isSymbol:
      let decl = getTypeSection(typ.symId)
      # A union constructor is partial; an RTTI object's vtable slot is
      # not zero (`vtables_backend` fills it in).
      if decl.kind == TypeY and hasPragma(decl.pragmas, UnionP) or
          hasRtti(typ.symId):
        return false
    elif not impl.childCursor.isDotToken:
      return false # an anonymous `object of Base`: its RTTI is not ours to judge
    result = true
    var obj = asObjectDecl(impl)
    while true:
      var fields = sub(obj.body) # past `(object`; bounds the walk
      skip fields                # the inheritance slot
      if fields.hasMore and not fields.isDotToken:
        var iter = initObjFieldIter()
        while nextField(iter, fields):
          let field = takeLocal(fields, SkipFinalParRi)
          if not field.val.isDotToken or not isZeroDefault(field.typ):
            return false
      if obj.parentType.isDotToken: break
      var parentImpl = obj.parentType
      if parentImpl.typeKind in {RefT, PtrT}:
        inc parentImpl
      obj = asObjectDecl(toTypeImpl(parentImpl))
  of TupleT, ClosureTupleT:
    result = true
    var field = sub(impl)
    while field.hasMore:
      if not isZeroDefault(getTupleFieldType(field)): return false
      skip field
  of ArrayT, DistinctT, SinkT, StaticT:
    result = isZeroDefault(impl.childCursor)
  else:
    result = false
