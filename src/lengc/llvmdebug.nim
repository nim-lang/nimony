#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## All LLVM DWARF debug-info generation: metadata infrastructure
## (DICompileUnit, DIFile, DISubprogram, DILocalVariable, DIGlobalVariable,
## #dbg_declare) and DWARF debug type metadata (DIBasicType, DIDerivedType,
## DICompositeType) generated from NIF type cursors.
##
## Included from llvmcodegen.nim after llvmgentypes — ``LLVMCode``,
## ``DebugInfo``, ``LLVMCurrentProc`` types, emit helpers, ``nifSymBaseName``
## and the type helpers (``typeSizeBits``, ``typeAlignBits``) are in scope.

# ---- DWARF encoding constants ----------------------------------------

const
  DW_ATE_address = 1
  DW_ATE_boolean = 2
  DW_ATE_float = 4
  DW_ATE_signed = 5
  DW_ATE_signed_char = 6
  DW_ATE_unsigned = 7
  DW_ATE_unsigned_char = 8

# ---- Debug metadata infrastructure -----------------------------------

proc addMetadata(c: var LLVMCode; node: string): int =
  ## Add a metadata node, return its ID.
  result = c.debug.nextMetadataId
  c.debug.metadata.add node
  inc c.debug.nextMetadataId

proc initDebugInfo(c: var LLVMCode; filename: string) =
  ## Debug metadata is initialized lazily: the first call to
  ## ``getOrCreateDIFile`` creates the DICompileUnit using the first
  ## real source file as its file reference.
  discard

proc bitsFromLLVMType(typ: LLType; defaultBits: int): int =
  if typ != nil and typ.kind == llInt: typ.intBits
  else: defaultBits

proc genDIBasicType(c: var LLVMCode; name: string; sizeBits,
    encoding: int): int =
  ## Get or create a DIBasicType metadata node. Cached by name+size+encoding.
  let key = name & ":" & $sizeBits & ":" & $encoding
  result = c.debug.diBasicTypeCache.getOrDefault(key)
  if result != 0: return
  result = c.addMetadata("!DIBasicType(name: \"" & name &
    "\", size: " & $sizeBits &
    ", encoding: " & $encoding & ")")
  c.debug.diBasicTypeCache[key] = result

proc getOrCreateDIFile(c: var LLVMCode; fid: FileId): int =
  ## Get or create a DIFile metadata node for the given FileId.
  let key = int(fid)
  if key in c.debug.fileIds:
    return c.debug.fileIds[key]
  let path = c.m.pool.filenames[fid]
  let (dir, name, ext) = splitFile(path)
  let fullName = name & ext
  let directory = if dir == "": getCurrentDir() else: absolutePath(dir)
  result = c.addMetadata("!DIFile(filename: \"" & fullName &
      "\", directory: \"" & directory & "\")")
  c.debug.fileIds[key] = result
  # First real source file → create the compile unit using this file
  if c.debug.cuId == 0:
    c.debug.cuId = c.addMetadata("distinct !DICompileUnit(language: DW_LANG_C99, file: !" &
      $result & ", producer: \"lengc\", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)")

proc dbgLocation(c: var LLVMCode; info: NifLineInfo): string =
  ## Return a `, !dbg !N` suffix for the given source location, or "" if invalid.
  if not info.isValid: return ""
  let rawInfo = info
  if not rawInfo.file.isValid: return ""
  let fileId = getOrCreateDIFile(c, rawInfo.file)
  let scopeId =
    if fileId == c.currentProc.subprogramFileId:
      c.currentProc.subprogramId
    else:
      c.addMetadata("!DILexicalBlockFile(scope: !" &
          $c.currentProc.subprogramId &
        ", file: !" & $fileId & ", discriminator: 0)")
  let locId = c.addMetadata("!DILocation(line: " & $rawInfo.line &
    ", column: " & $(rawInfo.col + 1) &
    ", scope: !" & $scopeId & ")")
  result = ", !dbg !" & $locId

proc createSubprogram(c: var LLVMCode; name: string; info: NifLineInfo): int =
  ## Create a DISubprogram metadata node for a function.
  var fileId = 0
  var line = 0
  if info.isValid:
    let rawInfo = info
    if rawInfo.file.isValid:
      fileId = getOrCreateDIFile(c, rawInfo.file)
      line = rawInfo.line
  c.currentProc.spName = name
  c.currentProc.spLine = line
  c.currentProc.spScopeLine = line
  c.currentProc.subprogramFileId = fileId
  # placeholder (content replaced later by finalizeSubprogram)
  result = c.addMetadata("")

proc finalizeSubprogram(c: var LLVMCode; spId: int;
                         paramDITypes, retainedNodes: seq[int]) =
  if spId == 0: return
  let fid = c.currentProc.subprogramFileId
  var typesStr = ""
  for i, pt in paramDITypes:
    if i > 0: typesStr.add ", "
    typesStr.add "!" & $pt
  let stId = c.addMetadata("!DISubroutineType(types: !{" & typesStr & "})")
  var rnList = ""
  for i, rn in retainedNodes:
    if i > 0: rnList.add ", "
    rnList.add "!" & $rn
  let rnId = c.addMetadata("!{" & rnList & "}")
  let finalSp = "distinct !DISubprogram(name: \"" & c.currentProc.spName &
    "\", scope: !" & $fid &
    ", file: !" & $fid &
    ", line: " & $c.currentProc.spLine &
    ", type: !" & $stId &
    ", scopeLine: " & $c.currentProc.spScopeLine &
    ", spFlags: DISPFlagDefinition, unit: !" & $c.debug.cuId &
    ", retainedNodes: !" & $rnId & ")"
  c.debug.metadata[spId] = finalSp

proc emitDbgDeclare(c: var LLVMCode; localName: string; symId: SymId;
                    wasName: string; info: NifLineInfo; diType: int = 0;
                    argNo: int = 0; llvmTyp: LLType = nil) =
  ## Emit a #dbg_declare for a local variable (as a raw intrinsic line).
  if not info.isValid: return
  let rawInfo = info
  if not rawInfo.file.isValid: return
  var useType = diType
  if useType == 0:
    let bits = bitsFromLLVMType(llvmTyp, c.bits)
    useType = genDIBasicType(c, "int " & $bits, bits, DW_ATE_signed)
  let debugName = if wasName.len > 0: wasName else: nifSymBaseName(c, symId)
  let fileId = getOrCreateDIFile(c, rawInfo.file)
  var varMetadata = "!DILocalVariable(name: \"" & debugName & "\""
  if argNo > 0:
    varMetadata.add ", arg: " & $argNo
  varMetadata.add ", scope: !" & $c.currentProc.subprogramId &
    ", file: !" & $fileId &
    ", line: " & $rawInfo.line &
    ", type: !" & $useType & ")"
  let varId = c.addMetadata(varMetadata)
  c.currentProc.retainedNodes.add varId
  let locId = c.addMetadata("!DILocation(line: " & $rawInfo.line &
    ", column: " & $(rawInfo.col + 1) &
    ", scope: !" & $c.currentProc.subprogramId & ")")
  c.emitRaw "#dbg_declare(ptr " & localName & ", !" & $varId &
      ", !DIExpression(), !" & $locId & ")"

proc emitGlobalDbgVar(c: var LLVMCode; name: string; varInfo: NifLineInfo;
                      symId: SymId; diType: int): string =
  if c.debug.cuId == 0: return ""
  var fileId = 0
  var line = 0
  if varInfo.isValid and varInfo.file.isValid:
    fileId = getOrCreateDIFile(c, varInfo.file)
    line = int(varInfo.line)
  if fileId == 0:
    fileId = c.currentProc.subprogramFileId
  if fileId == 0: return ""
  if diType == 0: return ""
  let displayName = nifSymBaseName(c, symId)
  let gvId = c.addMetadata("distinct !DIGlobalVariable(name: \"" & displayName &
    "\", scope: !" & $c.debug.cuId &
    ", file: !" & $fileId &
    ", line: " & $line &
    ", type: !" & $diType &
    ", isLocal: false, isDefinition: true)")
  let gveId = c.addMetadata("!DIGlobalVariableExpression(var: !" &
    $gvId & ", expr: !DIExpression())")
  c.debug.globalExprs.add gveId
  result = ", !dbg !" & $gveId

# ---- DWARF debug type generation -------------------------------------

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
        let b = intVal(n)
        if b != -1: bits = int(b)
        inc n
      result = genDIBasicType(c, "int " & $bits, bits, DW_ATE_signed)
      while n.hasMore: skip n
  of UT:
    n.into:
      var bits = c.bits
      if n.kind == IntLit:
        let b = intVal(n)
        if b != -1: bits = int(b)
        inc n
      result = genDIBasicType(c, "uint " & $bits, bits, DW_ATE_unsigned)
      while n.hasMore: skip n
  of CT:
    n.into:
      var bits = 8
      if n.kind == IntLit:
        let b = intVal(n)
        if b != -1: bits = int(b)
        inc n
      result = genDIBasicType(c, "char", bits, DW_ATE_signed_char)
      while n.hasMore: skip n
  of FT:
    n.into:
      var bits = 64
      if n.kind == IntLit:
        let b = intVal(n)
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
    let baseRef = if pt > 1: "!" & $pt else: "null"
    result = c.addMetadata("!DIDerivedType(tag: DW_TAG_pointer_type" &
      ", baseType: " & baseRef &
      ", size: " & $c.bits &
      ", align: " & $c.bits &
      ", dwarfAddressSpace: 0)")
    skip n
  of ProctypeT:
    # Opaque pointer — DWARF just needs the pointer type, not the signature
    result = c.addMetadata("!DIDerivedType(tag: DW_TAG_pointer_type" &
      ", baseType: null" &
      ", size: " & $c.bits &
      ", align: " & $c.bits &
      ", dwarfAddressSpace: 0)")
    skip n
  of FlexarrayT:
    # [0 x T] — treat as array with unknown bound
    n.into:
      let et = genDIType(c, n)
      let subrange = c.addMetadata("!DISubrange(count: -1)")
      let totalSize = 0 # flexarray has unknown size for DWARF
      result = c.addMetadata("!DICompositeType(tag: DW_TAG_array_type" &
        ", elements: !{!" & $subrange & "}" &
        ", baseType: !" & $et &
        ", size: " & $totalSize & ")")
      while n.hasMore: skip n
  of ArrayT:
    let totalSize = typeSizeBits(c, n) # capture before entering
    n.into:
      let et = genDIType(c, n)
      var sz = 0
      if n.kind == IntLit:
        sz = int(intVal(n))
        skip n
      elif n.kind == UIntLit:
        sz = int(uintVal(n))
        skip n
      let subrange = c.addMetadata("!DISubrange(count: " & $sz & ")")
      result = c.addMetadata("!DICompositeType(tag: DW_TAG_array_type" &
        ", elements: !{!" & $subrange & "}" &
        ", baseType: !" & $et &
        ", size: " & $totalSize & ")")
      while n.hasMore: skip n
  of EnumT:
    let enumSize = typeSizeBits(c, n) # capture BEFORE n.into
    n.into:
      let base = genDIType(c, n) # underlying integer type
      var enumerators: seq[int] = @[]
      while n.hasMore:
        if n.substructureKind == EfldU:
          n.into:
            let enumName = nifSymBaseName(c, n.symId)
            inc n
            var val = 0
            if n.kind == IntLit:
              val = int(intVal(n))
              inc n
            elif n.kind == UIntLit:
              val = int(uintVal(n))
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

# ---- Pointer type ----------------------------------------------------

proc genDIPointerType(c: var LLVMCode; pointeeType: Cursor): int =
  let pt = genDITypeReadOnly(c, pointeeType)
  let ptStr = if pt != 0: "!$" & $pt else: "null"
  result = c.addMetadata("!DIDerivedType(tag: DW_TAG_pointer_type" &
    ", baseType: " & ptStr &
    ", size: " & $c.bits &
    ", align: " & $c.bits &
    ", dwarfAddressSpace: 0)")

# ---- Type for a named symbol -----------------------------------------

proc genDITypeForSymbol(c: var LLVMCode; symId: SymId): int =
  result = c.debug.diTypeCache.getOrDefault(symId)
  if result == -1:
    # Cycle: emit placeholder and cache it
    let d = c.m.getDeclOrNil(symId)
    let typeName = if d != nil and d.kind == TypeY:
                     nifSymBaseName(c, asTypeDecl(d.pos).name.symId)
                   else: nifSymBaseName(c, symId)
    result = c.addMetadata("!DICompositeType(tag: DW_TAG_structure_type" &
      ", name: \"" & typeName & "\", elements: !{}, size: 0)")
    c.debug.diTypeCache[symId] = result
    return result
  if result != 0: return

  let d = c.m.getDeclOrNil(symId)
  if d != nil and d.kind == TypeY:
    let decl = asTypeDecl(d.pos)
    let typeName = nifSymBaseName(c, decl.name.symId)
    # Set sentinel before recursing to break cycles
    c.debug.diTypeCache[symId] = -1
    let underlying = genDITypeReadOnly(c, decl.body)
    if underlying > 1:
      if decl.body.typeKind in {ObjectT, UnionT}:
        # DICompositeType already carries the name; no typedef wrapper needed.
        # genDICompositeType may have returned a placeholder — it will update
        # the metadata in-place when it builds the full type.
        result = underlying
      else:
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

proc genDIUnionType(c: var LLVMCode; n: var Cursor): int
proc genDIAnonObject(c: var LLVMCode; n: var Cursor): int

proc diElemsList(members: seq[int]): string =
  result = ""
  for i, m in members:
    if i > 0: result.add ", "
    result.add "!" & $m

proc addFieldMember(c: var LLVMCode; fd: FieldDecl; members: var seq[int];
                    fieldOffset: var int) =
  let fieldName = nifSymBaseName(c, fd.name.symId)
  let fieldType = genDITypeReadOnly(c, fd.typ)
  if fieldType == 0: return
  let fieldSize = typeSizeBits(c, fd.typ)
  let fieldAlign = if fd.typ.typeKind in {PtrT, AptrT, ProctypeT}:
                    c.bits else: min(fieldSize, c.bits)
  if fieldAlign > 0:
    fieldOffset = ((fieldOffset + fieldAlign - 1) div fieldAlign) * fieldAlign
  let fInfo = fd.name.info
  var fFileId = 0
  var fLine = 0
  if fInfo.isValid and fInfo.file.isValid:
    fFileId = getOrCreateDIFile(c, fInfo.file)
    fLine = int(fInfo.line)
  var mStr = "!DIDerivedType(tag: DW_TAG_member" &
    ", name: \"" & fieldName & "\""
  if fFileId != 0:
    mStr.add ", file: !" & $fFileId & ", line: " & $fLine
  mStr.add ", baseType: !" & $fieldType &
    ", size: " & $fieldSize &
    ", align: " & $fieldAlign
  if fieldOffset > 0:
    mStr.add ", offset: " & $fieldOffset
  mStr.add ")"
  members.add c.addMetadata(mStr)
  fieldOffset += fieldSize

proc addUnionMember(c: var LLVMCode; n: var Cursor; members: var seq[int];
                    fieldOffset: var int) =
  ## Add the embedded union ``n`` (UnionT) as a single member whose baseType
  ## is an anonymous DICompositeType union. Consumes ``n``.
  let uSize = typeSizeBits(c, n)
  let uAlign = typeAlignBits(c, n)
  let udi = genDIUnionType(c, n)
  if uAlign > 0:
    fieldOffset = ((fieldOffset + uAlign - 1) div uAlign) * uAlign
  if udi != 0:
    var mStr = "!DIDerivedType(tag: DW_TAG_member" &
      ", baseType: !" & $udi &
      ", size: " & $uSize &
      ", align: " & $uAlign
    if fieldOffset > 0:
      mStr.add ", offset: " & $fieldOffset
    mStr.add ")"
    members.add c.addMetadata(mStr)
  fieldOffset += uSize

proc genDIAnonObject(c: var LLVMCode; n: var Cursor): int =
  ## Generate an anonymous DICompositeType struct for an inline object body.
  var members: seq[int] = @[]
  var fieldOffset = 0
  let oSize = typeSizeBits(c, n)
  let oAlign = typeAlignBits(c, n)
  let kind = n.typeKind
  n.into:
    if kind == ObjectT:
      if n.kind == DotToken: inc n
      elif n.kind == Symbol: inc n
    while n.hasMore:
      if n.substructureKind == FldU:
        var fd = takeFieldDecl(n)
        addFieldMember(c, fd, members, fieldOffset)
      elif n.typeKind == UnionT:
        addUnionMember(c, n, members, fieldOffset)
      else:
        skip n
  result = c.addMetadata("!DICompositeType(tag: DW_TAG_structure_type" &
    ", elements: !{" & diElemsList(members) & "}" &
    ", size: " & $oSize &
    ", align: " & $oAlign & ")")

proc genDIUnionType(c: var LLVMCode; n: var Cursor): int =
  ## Generate an anonymous DICompositeType union for an inline union body.
  ## Each branch (nested object / field) becomes an overlapping member.
  var members: seq[int] = @[]
  let uSize = typeSizeBits(c, n)
  let uAlign = typeAlignBits(c, n)
  n.into:
    while n.hasMore:
      if n.substructureKind == FldU:
        var fd = takeFieldDecl(n)
        var off = 0
        addFieldMember(c, fd, members, off)
      elif n.typeKind == ObjectT:
        let bdi = genDIAnonObject(c, n)
        if bdi != 0:
          members.add c.addMetadata("!DIDerivedType(tag: DW_TAG_member" &
            ", baseType: !" & $bdi & ")")
      elif n.typeKind == UnionT:
        let ndi = genDIUnionType(c, n)
        if ndi != 0:
          members.add c.addMetadata("!DIDerivedType(tag: DW_TAG_member" &
            ", baseType: !" & $ndi & ")")
      else:
        skip n
  result = c.addMetadata("!DICompositeType(tag: DW_TAG_union_type" &
    ", elements: !{" & diElemsList(members) & "}" &
    ", size: " & $uSize &
    ", align: " & $uAlign & ")")

proc genDICompositeType(c: var LLVMCode; n: var Cursor): int =
  ## Generate DICompositeType for ObjectT or UnionT.
  ## Walks ``decl.body`` (not the inline cursor) — matches
  ## ``addObjectFieldsLLVM`` / ``genObjectBodyLLVM`` pattern exactly.
  let td = tracebackTypeC(c.m, n)
  let decl = asTypeDecl(td)
  let symId = decl.name.symId

  # Already fully built? Return immediately.
  if symId in c.debug.compositeTypeDone:
    result = c.debug.diTypeCache.getOrDefault(symId)
    skip n
    return

  # Save the old cache value (could be 0, -1 sentinel, or a placeholder
  # metadata ID from a previous genDITypeForSymbol / genDICompositeType call).
  let oldCached = c.debug.diTypeCache.getOrDefault(symId)

  # Set sentinel to break cycles during field processing.
  # Even if oldCached == -1 (genDITypeForSymbol is processing this type),
  # we still build the full type here. The -1 sentinel already handles
  # self-references: genDITypeForSymbol will create a placeholder when it
  # sees -1, and we update that placeholder in-place at the end.
  c.debug.diTypeCache[symId] = -1

  let tag = if decl.body.typeKind == UnionT: "DW_TAG_union_type"
            else: "DW_TAG_structure_type"
  let typeName = nifSymBaseName(c, symId)
  let totalSize = typeSizeBits(c, decl.body)
  let totalAlign = typeAlignBits(c, decl.body)

  # Source location for the type declaration
  let declInfo = decl.name.info
  var declFileId = 0
  var declLine = 0
  if declInfo.isValid and declInfo.file.isValid:
    declFileId = getOrCreateDIFile(c, declInfo.file)
    declLine = int(declInfo.line)

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
        addFieldMember(c, fd, members, fieldOffset)
      elif body.typeKind == UnionT:
        addUnionMember(c, body, members, fieldOffset)
      elif body.typeKind == ObjectT:
        skip body
      else:
        skip body

  var membersStr = ""
  for i, m in members:
    if i > 0: membersStr.add ", "
    membersStr.add "!" & $m

  var realTypeStr = "!DICompositeType(tag: " & tag &
    ", name: \"" & typeName & "\""
  if declFileId != 0:
    realTypeStr.add ", file: !" & $declFileId & ", line: " & $declLine
  realTypeStr.add ", size: " & $totalSize &
    ", align: " & $totalAlign &
    ", elements: !{" & membersStr & "})"

  # Check if a placeholder was created during field processing (self-referential
  # type, e.g. ``type Node = object; next: ptr Node``).
  let postFieldCached = c.debug.diTypeCache.getOrDefault(symId)

  # Update pre-existing placeholder (from genDITypeForSymbol or recursive call)
  if oldCached > 1:
    c.debug.metadata[oldCached] = realTypeStr
    result = oldCached
  # Also update any placeholder created during field processing
  if postFieldCached > 1 and postFieldCached != oldCached:
    c.debug.metadata[postFieldCached] = realTypeStr
    if result == 0:
      result = postFieldCached
  # No placeholder exists yet — create fresh metadata
  if result == 0:
    result = c.addMetadata(realTypeStr)

  c.debug.diTypeCache[symId] = result
  c.debug.compositeTypeDone.incl symId

  # Consume the caller's inline cursor
  skip n
