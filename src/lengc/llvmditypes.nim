#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Generates DWARF debug type metadata (DIBasicType, DIDerivedType,
## DICompositeType, etc.) from NIF type cursors.
##
## Included from llvmcodegen.nim after llvmgentypes — all type helpers
## (typeSizeBits, typeAlignBits) and the DIBuilder string helpers
## (addMetadata, genDIBasicType) are in scope.

# ---- Forward decls ---------------------------------------------------

proc genDIType*(c: var LLVMCode; n: Cursor): int
proc genDITypeForSymbol(c: var LLVMCode; symId: SymId): int
proc genDICompositeType(c: var LLVMCode; n: var Cursor): int

# ---- Helper: read-only cursor ---------------------------------------

proc genDITypeReadOnly(c: var LLVMCode; n: Cursor): int =
  ## Like genDIType but does not advance the cursor.
  var nn = n
  result = genDIType(c, nn)

# ---- Main dispatcher ------------------------------------------------

proc genDITypeImpl(c: var LLVMCode; n: var Cursor): int =
  case n.typeKind
  of VoidT:
    result = 0
    skip n
  of IT:
    n.into:
      var bits = c.bits
      if n.kind == IntLit:
        let b = pool.integers[n.intId]
        if b != -1: bits = int(b)
        inc n
      result = genDIBasicType(c, "int " & $bits, bits, DW_ATE_signed)
      while n.hasMore: skip n
  of UT:
    n.into:
      var bits = c.bits
      if n.kind == IntLit:
        let b = pool.integers[n.intId]
        if b != -1: bits = int(b)
        inc n
      result = genDIBasicType(c, "uint " & $bits, bits, DW_ATE_unsigned)
      while n.hasMore: skip n
  of CT:
    n.into:
      var bits = 8
      if n.kind == IntLit:
        let b = pool.integers[n.intId]
        if b != -1: bits = int(b)
        inc n
      result = genDIBasicType(c, "char", bits, DW_ATE_signed_char)
      while n.hasMore: skip n
  of FT:
    n.into:
      var bits = 64
      if n.kind == IntLit:
        let b = pool.integers[n.intId]
        if b != -1: bits = int(b)
        inc n
      let name = if bits == 32: "float" elif bits == 128: "fp128" else: "double"
      result = genDIBasicType(c, name, bits, DW_ATE_float)
      while n.hasMore: skip n
  of BoolT:
    n.into:
      while n.hasMore: skip n
    result = genDIBasicType(c, "bool", 8, DW_ATE_boolean)
  of PtrT, AptrT:
    let pointee = n.firstSon
    let pt = genDITypeReadOnly(c, pointee)
    # Only metadata IDs > 1 are valid DITypes (0 = null, 1 = DICompileUnit)
    let baseRef = if pt > 1: "!" & $pt else: "null"
    result = c.addMetadata("!DIDerivedType(tag: DW_TAG_pointer_type" &
      ", baseType: " & baseRef &
      ", size: " & $c.bits &
      ", align: " & $c.bits & ")")
    skip n
  of ProctypeT:
    # Opaque pointer — DWARF just needs the pointer type, not the signature
    result = c.addMetadata("!DIDerivedType(tag: DW_TAG_pointer_type" &
      ", baseType: null" &
      ", size: " & $c.bits &
      ", align: " & $c.bits & ")")
    skip n
  of FlexarrayT:
    # [0 x T] — treat as array with unknown bound
    n.into:
      let et = genDIType(c, n)
      let subrange = c.addMetadata("!DISubrange(count: -1)")
      let totalSize = 0  # flexarray has unknown size for DWARF
      result = c.addMetadata("!DICompositeType(tag: DW_TAG_array_type" &
        ", elements: !{!" & $subrange & "}" &
        ", baseType: !" & $et &
        ", size: " & $totalSize & ")")
      while n.hasMore: skip n
  of ArrayT:
    let totalSize = typeSizeBits(c, n)  # capture before entering
    n.into:
      let et = genDIType(c, n)
      var sz = 0
      if n.kind == IntLit:
        sz = int(pool.integers[n.intId])
        skip n
      elif n.kind == UIntLit:
        sz = int(pool.uintegers[n.uintId])
        skip n
      let subrange = c.addMetadata("!DISubrange(count: " & $sz & ")")
      result = c.addMetadata("!DICompositeType(tag: DW_TAG_array_type" &
        ", elements: !{!" & $subrange & "}" &
        ", baseType: !" & $et &
        ", size: " & $totalSize & ")")
      while n.hasMore: skip n
  of EnumT:
    let enumSize = typeSizeBits(c, n)   # capture BEFORE n.into
    n.into:
      let base = genDIType(c, n)  # underlying integer type
      var enumerators: seq[int] = @[]
      while n.hasMore:
        if n.substructureKind == EfldU:
          n.into:
            let enumName = pool.syms[n.symId]
            inc n
            var val = 0
            if n.kind == IntLit:
              val = int(pool.integers[n.intId])
              inc n
            elif n.kind == UIntLit:
              val = int(pool.uintegers[n.uintId])
              inc n
            let en = c.addMetadata("!DIEnumerator(name: \"" & enumName &
              "\", value: " & $val & ")")
            enumerators.add en
            while n.hasMore: skip n
        else:
          skip n
      var enumElems = ""
      for i, e in enumerators:
        if i > 0: enumElems.add ", "
        enumElems.add "!" & $e
      result = c.addMetadata("!DICompositeType(tag: DW_TAG_enumeration_type" &
        ", baseType: !" & $base &
        ", elements: !{" & enumElems & "}" &
        ", size: " & $enumSize & ")")
      while n.hasMore: skip n
  of ObjectT, UnionT:
    result = genDICompositeType(c, n)
    # genDICompositeType always consumes n (skip n at end, or skip n on cache hit)
  of NoType:
    if n.kind == Symbol:
      result = genDITypeForSymbol(c, n.symId)
      inc n
    elif n.kind == DotToken:
      result = 0
      inc n
    else:
      result = 0
      skip n
  of VarargsT:
    result = 0
    skip n
  of ParamsT:
    result = 0
    skip n
  else:
    result = 0
    skip n

# ---- Public dispatcher ----------------------------------------------

proc genDIType*(c: var LLVMCode; n: Cursor): int =
  var nn = n
  result = genDITypeImpl(c, nn)

# Size/align come from llvmgentypes (included before us).

# ---- Pointer type ----------------------------------------------------

proc genDIPointerType(c: var LLVMCode; pointeeType: Cursor): int =
  let pt = genDITypeReadOnly(c, pointeeType)
  let ptStr = if pt != 0: "!$" & $pt else: "null"
  result = c.addMetadata("!DIDerivedType(tag: DW_TAG_pointer_type" &
    ", baseType: " & ptStr &
    ", size: " & $c.bits &
    ", align: " & $c.bits & ")")

# ---- Type for a named symbol -----------------------------------------

proc genDITypeForSymbol(c: var LLVMCode; symId: SymId): int =
  result = c.debug.diTypeCache.getOrDefault(symId)
  if result == -1:
    # Cycle: emit placeholder and cache it
    let d = c.m.getDeclOrNil(symId)
    let typeName = if d != nil and d.kind == TypeY:
                     pool.syms[asTypeDecl(d.pos).name.symId]
                   else: pool.syms[symId]
    result = c.addMetadata("!DICompositeType(tag: DW_TAG_structure_type" &
      ", name: \"" & typeName & "\", elements: !{}, size: 0)")
    c.debug.diTypeCache[symId] = result
    return result
  if result != 0: return

  let d = c.m.getDeclOrNil(symId)
  if d != nil and d.kind == TypeY:
    let decl = asTypeDecl(d.pos)
    let typeName = pool.syms[decl.name.symId]
    # Set sentinel before recursing to break cycles
    c.debug.diTypeCache[symId] = -1
    let underlying = genDITypeReadOnly(c, decl.body)
    if underlying > 1:
      result = c.addMetadata("!DIDerivedType(tag: DW_TAG_typedef" &
        ", name: \"" & typeName & "\"" &
        ", baseType: !" & $underlying & ")")
    else:
      result = 0
    c.debug.diTypeCache[symId] = result
  elif d != nil:
    result = 0
  else:
    result = 0

# ---- Composite type (struct / union) ----------------------------------

proc genDICompositeType(c: var LLVMCode; n: var Cursor): int =
  ## Generate DICompositeType for ObjectT or UnionT.
  ## Walks ``decl.body`` (not the inline cursor) — matches
  ## ``addObjectFieldsLLVM`` / ``genObjectBodyLLVM`` pattern exactly.
  let td = tracebackTypeC(n)
  let decl = asTypeDecl(td)
  let symId = decl.name.symId

  # Cache check — consume inline cursor on hit
  result = c.debug.diTypeCache.getOrDefault(symId)
  if result == -1:
    let tag = if decl.body.typeKind == UnionT: "DW_TAG_union_type"
              else: "DW_TAG_structure_type"
    result = c.addMetadata("!DICompositeType(tag: " & tag &
      ", name: \"" & pool.syms[symId] & "\"" &
      ", elements: !{}" &
      ", size: " & $typeSizeBits(c, decl.body) & ")")
    c.debug.diTypeCache[symId] = result
    skip n
    return result
  if result != 0:
    skip n
    return result

  # Placeholder to break cycles
  c.debug.diTypeCache[symId] = -1

  let tag = if decl.body.typeKind == UnionT: "DW_TAG_union_type"
            else: "DW_TAG_structure_type"
  let typeName = pool.syms[symId]
  let totalSize = typeSizeBits(c, decl.body)

  # Walk the canonical body from the declaration (pattern from addObjectFieldsLLVM)
  var members: seq[int] = @[]
  var fieldOffset = 0
  var body = decl.body
  let k = body.typeKind
  body.into:
    if k == ObjectT:
      if body.kind == DotToken:
        inc body
      elif body.kind == Symbol:
        let baseDi = genDITypeForSymbol(c, body.symId)
        if baseDi != 0:
          members.add c.addMetadata("!DIDerivedType(tag: DW_TAG_inheritance" &
            ", baseType: !" & $baseDi &
            ", size: " & $typeSizeBits(c, body) &
            ", offset: 0, flags: 0)")
        inc body

    while body.hasMore:
      if body.substructureKind == FldU:
        var fd = takeFieldDecl(body)
        let fieldName = pool.syms[fd.name.symId]
        let fieldType = genDITypeReadOnly(c, fd.typ)
        if fieldType != 0:
          let fieldSize = typeSizeBits(c, fd.typ)
          let fieldAlign = if fd.typ.typeKind in {PtrT, AptrT, ProctypeT}:
                            c.bits else: min(fieldSize, c.bits)
          if fieldAlign > 0:
            fieldOffset = ((fieldOffset + fieldAlign - 1) div fieldAlign) * fieldAlign
          members.add c.addMetadata("!DIDerivedType(tag: DW_TAG_member" &
            ", name: \"" & fieldName & "\"" &
            ", baseType: !" & $fieldType &
            ", size: " & $fieldSize &
            ", offset: " & $fieldOffset & ")")
          fieldOffset += fieldSize
      elif body.typeKind == UnionT:
        discard genUnionBodyLLVM(c, body)
      elif body.typeKind == ObjectT:
        skip body
      else:
        skip body

  var membersStr = ""
  for i, m in members:
    if i > 0: membersStr.add ", "
    membersStr.add "!" & $m

  result = c.addMetadata("!DICompositeType(tag: " & tag &
    ", name: \"" & typeName & "\"" &
    ", elements: !{" & membersStr & "}" &
    ", size: " & $totalSize & ")")
  c.debug.diTypeCache[symId] = result

  # Consume the caller's inline cursor
  skip n

