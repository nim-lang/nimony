#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Default values for hexer-synthesized object constructors.

`(oconstr T ...)` is **total**: it mentions every field of `T`. Nimsem
guarantees that for every constructor the user writes — see
`buildDefaultObjConstr` in `sem.nim`, which fills each field the source
omitted with `default(FieldType)` — and a Leng consumer is entitled to
rely on it. The C back end would forgive a partial one (a designated
initializer zeroes what it does not mention), but the native back end
stores exactly the fields the constructor lists and zeroes nothing, so
there an omitted field is whatever the storage happened to hold.

Hexer passes synthesize constructors of their own, for object types they
invent: the coroutine frame in `coro_transform`, the closure environment
in `lambdalifting`. Those must keep the invariant, and they cannot borrow
nimsem's machinery to do it: `default(T)` there is an overloaded *call*
resolved against `system/defaults.nim`, and by the time a hexer pass runs,
sem and the desugaring, duplifier and destroyer passes are long done —
there is no introducing a call, let alone the loop that
`default(array[I, T])` expands into. So this module rebuilds the same
value directly, as a literal expression tree, and every emitted form is
one that survives untouched to `lengcgen`.

The result matches nimsem's `default(T)` shape for shape, which for every
type Nim can default-construct is also the type's all-zero bit pattern —
the one state in which a `=destroy` is a no-op, which is what makes it
safe to store into a field whose real value has not been assigned yet.

Not handled, deliberately: `openarray`/`uarray`, `varargs`, `void`,
concepts and the meta types. None of them can be a field, so reaching one
means a caller passed a type that was never a location; that is a bug in
the caller and is reported as one rather than papered over with zero.
]##

import std / assertions
when defined(nimony):
  {.feature: "lenientnils".}
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typeprops, expreval,
  xints, reporters]

proc addDefaultValue*(dest: var TokenBuf; typ: Cursor; info: NifLineInfo; ptrSize: int)

proc bitsOf(impl: Cursor; ptrSize: int): int =
  ## Width of an `(i|u|f BITS)` type. `-1` is `int`/`uint`/`float`'s
  ## "whatever the target's pointer is", spelled that way by sem because
  ## the target is not known there; here it is.
  let bits = typebits(impl.childCursor.load)
  result = if bits > 0: bits else: ptrSize * 8

proc addSuffixedZero(dest: var TokenBuf; impl: Cursor; info: NifLineInfo;
                     ptrSize: int; prefix: char) =
  ## `(suf 0 "i64")` and friends — a numeric literal plus the suffix that
  ## pins its width, exactly as sem writes the literals in `defaults.nim`.
  let bits = bitsOf(impl, ptrSize)
  dest.addParLe(SufX, info)
  case prefix
  of 'i': dest.addIntLit(0, info)
  of 'u': dest.addUIntLit(0'u64, info)
  else: dest.addFloatLit(0.0, info)
  dest.addStrLit(prefix & $bits, info)
  dest.addParRi()

proc addNil(dest: var TokenBuf; typ: Cursor; info: NifLineInfo) =
  ## `(nil T)`. The type slot is not decoration: `derefs.nim` types every
  ## `nil` the frontend produces and `lengcgen` keeps it, because that is
  ## what tells `intramodinliner` which pointer it is splicing.
  dest.addParLe(NilX, info)
  dest.addSubtree typ
  dest.addParRi()

proc addDefaultObjConstr(dest: var TokenBuf; typ, impl: Cursor;
                         info: NifLineInfo; ptrSize: int)

proc addDefaultField(dest: var TokenBuf; n: var Cursor; info: NifLineInfo;
                     ptrSize: int; depth: int; isUnion: bool) =
  let field = takeLocal(n, SkipFinalParRi)
  if isUnion:
    # A union has ONE active member, and sibling designated initializers
    # for the others would zero it back out under C's last-wins rule. So a
    # union constructor is the one that stays partial — mirroring nimsem,
    # which skips the unset members here for the same reason.
    return
  dest.addParLe(KvU, info)
  dest.addSymUse(field.name.symId, info)
  if not field.val.isDotToken:
    # `x: int = -1` — the field declares its own default and construction
    # owes the reader that value, not the type's zero. Already semchecked
    # at the type declaration, and hexer sees no generic types, so it can
    # go out as it stands.
    dest.addSubtree field.val
  else:
    addDefaultValue(dest, field.typ, info, ptrSize)
  if depth != 0:
    dest.addIntLit(depth, info)
  dest.addParRi()

proc addDefaultFields(dest: var TokenBuf; n: var Cursor; info: NifLineInfo;
                      ptrSize: int; depth = 0; isUnion = false) =
  var iter = initObjFieldIter()
  while nextField(iter, n, keepCase = true):
    if n.substructureKind == CaseU:
      var body = n
      body = sub(body) # bound the branch walk; `body` is a copy
      addDefaultField(dest, body, info, ptrSize, depth, isUnion = false)
      # With no discriminator supplied there is no branch to select, so
      # take the first one, as nimsem's `fieldsPresentInBranch` does for
      # its `bestBranch`. The branches overlap in storage; filling more
      # than one would have them overwrite each other.
      var branch = default(Cursor)
      while body.hasMore:
        case body.substructureKind
        of OfU:
          branch = sub(body)
          skip branch # the `of` values
          break
        of ElseU:
          branch = sub(body)
          break
        else:
          error "illformed AST inside case object: ", body
      if branch != default(Cursor) and branch.hasMore:
        var fields = sub(branch) # past the branch's `(stmts`
        addDefaultFields(dest, fields, info, ptrSize, depth)
      skip n
    else:
      addDefaultField(dest, n, info, ptrSize, depth, isUnion)

proc addDefaultObjConstr(dest: var TokenBuf; typ, impl: Cursor;
                         info: NifLineInfo; ptrSize: int) =
  ## `(oconstr T (kv f <default>)*)` over the type's own fields and every
  ## inherited one, each tagged with its inheritance depth. The vtable
  ## slot of an RTTI type is not among them: it is not a declared field,
  ## and `vtables_backend` splices it into every `oconstr` — including the
  ## nested ones this proc emits — after cps has run.
  dest.addParLe(OconstrX, info)
  dest.addSubtree typ
  let obj = asObjectDecl(impl)
  var parentType = obj.parentType
  var depth = 1
  while not parentType.isDotToken:
    var parentImpl = parentType
    if parentImpl.typeKind in {RefT, PtrT}:
      inc parentImpl
    parentImpl = toTypeImpl(parentImpl)
    if parentImpl.typeKind != ObjectT:
      error "invalid parent object type: ", parentType
    let parent = asObjectDecl(parentImpl)
    var currentField = sub(parent.body) # past `(object`; bounds the walk
    skip currentField                   # the inheritance slot
    if currentField.hasMore and not currentField.isDotToken:
      addDefaultFields(dest, currentField, info, ptrSize, depth)
    parentType = parent.parentType
    inc depth
  var currentField = sub(obj.body) # past `(object`; bounds the walk
  skip currentField                # the inheritance slot
  if currentField.hasMore and not currentField.isDotToken:
    var isUnion = false
    if typ.isSymbol:
      let decl = getTypeSection(typ.symId)
      isUnion = decl.kind == TypeY and hasPragma(decl.pragmas, UnionP)
    addDefaultFields(dest, currentField, info, ptrSize, isUnion = isUnion)
  dest.addParRi()

proc addDefaultValue*(dest: var TokenBuf; typ: Cursor; info: NifLineInfo; ptrSize: int) =
  ## Emit the default value of `typ` as a single expression.
  ##
  ## `typ` is passed through untouched wherever a type slot is written, so
  ## a nominal type stays the symbol it was named by and `lengcgen` still
  ## maps it to the declared Leng type.
  let impl = toTypeImpl(typ)
  case impl.typeKind
  of IntT:
    addSuffixedZero(dest, impl, info, ptrSize, 'i')
  of UIntT:
    addSuffixedZero(dest, impl, info, ptrSize, 'u')
  of FloatT:
    addSuffixedZero(dest, impl, info, ptrSize, 'f')
  of CharT:
    dest.addCharLit('\0', info)
  of BoolT:
    dest.addParPair(FalseX, info)
  of EnumT, HoleyEnumT, AnumT:
    # `default(T: enum)` is `low(T)`, which for a holey enum is its first
    # declared value and not necessarily 0.
    var err = false
    let lo = asSigned(enumBounds(impl).lo, err)
    if err:
      error "cannot determine the first value of enum type: ", typ
    else:
      dest.addIntLit(lo, info)
  of PtrT, RefT, RoutineTypes, CstringT, PointerT, NiltT, MutT, OutT, LentT:
    addNil(dest, typ, info)
  of SetT:
    var err = false
    let size = asSigned(bitsetSizeInBytes(impl.childCursor), err)
    if err:
      error "invalid set element type: ", typ
    elif size in [1'i64, 2, 4, 8]:
      # Small sets are a single unsigned word in Leng.
      dest.addUIntLit(0'u64, info)
    else:
      # Bigger ones are an array of bytes; `lengcgen` derives that array
      # type from the very `(set T)` written here, so the constructor and
      # its type slot cannot disagree.
      dest.addParLe(AconstrX, info)
      dest.addSubtree typ
      for _ in 0 ..< size:
        dest.addUIntLit(0'u64, info)
      dest.addParRi()
  of ObjectT:
    addDefaultObjConstr(dest, typ, impl, info, ptrSize)
  of TupleT, ClosureTupleT:
    dest.addParLe(TupconstrX, info)
    dest.addSubtree typ
    var field = sub(impl)
    while field.hasMore:
      addDefaultValue(dest, getTupleFieldType(field), info, ptrSize)
      skip field
    dest.addParRi()
  of ArrayT:
    var err = false
    let len = asSigned(getArrayLen(impl), err)
    if err:
      error "cannot determine array length of type: ", typ
    else:
      dest.addParLe(AconstrX, info)
      dest.addSubtree typ
      let elem = impl.childCursor
      for _ in 0 ..< len:
        addDefaultValue(dest, elem, info, ptrSize)
      dest.addParRi()
  of RangetypeT:
    # `system/defaults.nim` has no `default` for a range type, so nimsem
    # never reaches this; a range-typed location always carries an
    # explicit initializer. Emit the lower bound anyway — it is the only
    # value of the type that is guaranteed to be in it.
    var lo = impl.childCursor
    skip lo # the base type
    dest.addSubtree lo
  of DistinctT:
    # A `distinct T` is `T`'s representation under another name.
    dest.addParLe(DconvX, info)
    dest.addSubtree typ
    addDefaultValue(dest, impl.childCursor, info, ptrSize)
    dest.addParRi()
  of SinkT, StaticT:
    addDefaultValue(dest, impl.childCursor, info, ptrSize)
  else:
    error "cannot build a default value for type: ", typ
