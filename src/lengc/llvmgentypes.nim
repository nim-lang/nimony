#
#
#           Leng Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvmcodegen.nim
# Generates LLVM IR types from Leng types.

proc integralBitsLLVM(t: Cursor; c: LLVMCode): int {.inline.} =
  let res = intVal(t)
  if res == -1: c.bits else: int(res)

proc genTypeLLVM(c: var LLVMCode; n: var Cursor): LLType =
  ## Returns the LLType for a NIF type node.
  case n.typeKind
  of VoidT:
    result = c.prim.voidT
    skip n
  of IT:
    n.into:
      if n.kind == IntLit:
        result = c.llIntBits(integralBitsLLVM(n, c))
        inc n
      else:
        result = c.llIntBits(c.bits)
      while n.hasMore:
        skip n
  of UT:
    n.into:
      if n.kind == IntLit:
        result = c.llIntBits(integralBitsLLVM(n, c))
        inc n
      else:
        result = c.llIntBits(c.bits)
      while n.hasMore:
        skip n
  of FT:
    n.into:
      var bits = 64
      if n.kind == IntLit:
        bits = integralBitsLLVM(n, c)
        inc n
      while n.hasMore:
        skip n
      result = if bits == 32: c.prim.f32 elif bits == 128: newLLFloatType(128)
               else: c.prim.f64
  of BoolT:
    n.into:
      while n.hasMore:
        skip n
    result = c.prim.i8 # Use i8 for bool to match C ABI (i1 has different ABI)
  of CT:
    n.into:
      if n.kind == IntLit:
        result = c.llIntBits(integralBitsLLVM(n, c))
        inc n
      else:
        result = c.prim.i8
      while n.hasMore:
        skip n
  of NoType:
    if n.kind == Symbol:
      let d = c.m.getDeclOrNil(n.symId)
      if d != nil and d.kind == TypeY:
        let decl = asTypeDecl(d.pos)
        if decl.body.typeKind == ProctypeT:
          result = c.prim.ptrT
          inc n
          return
      let name = mangleToC(c.m.pool.syms[n.symId])
      result = LLType(kind: llStruct, name: name) # named reference → "%name"
      inc n
    elif n.kind == DotToken:
      result = c.prim.voidT
      inc n
    else:
      error c.m, "node is not a type: ", n
  of PtrT, AptrT:
    result = c.prim.ptrT
    skip n
  of FlexarrayT:
    n.into:
      let elemType = genTypeLLVM(c, n)
      result = newLLArrayType(0, elemType)
      while n.hasMore: skip n
  of ProctypeT:
    result = c.prim.ptrT
    skip n
  of VarargsT:
    result = c.prim.ptrT # placeholder; handled specially in signatures
    skip n
  of EnumT:
    n.into:
      result = genTypeLLVM(c, n)
      while n.hasMore:
        skip n
  of ArrayT:
    n.into:
      let elemType = genTypeLLVM(c, n)
      var sz = 0
      if n.kind == IntLit:
        sz = int(intVal(n))
        skip n
      elif n.kind == UIntLit:
        sz = int(uintVal(n))
        skip n
      result = newLLArrayType(sz, elemType)
      while n.hasMore: skip n
  of ObjectT, UnionT:
    let typeDecl = tracebackTypeC(c.m, n)
    if not cursorIsNil(typeDecl) and typeDecl.stmtKind == TypeS:
      let decl = asTypeDecl(typeDecl)
      if decl.name.kind == SymbolDef:
        discard c.m.getDeclOrNil(decl.name.symId)
        result = LLType(kind: llStruct, name: mangleToC(c.m.pool.syms[
            decl.name.symId]))
      else:
        result = c.prim.ptrT
    else:
      result = c.prim.ptrT
    skip n
  of ParamsT:
    error c.m, "params type not allowed in expression context: ", n

proc genTypeLLVMReadOnly(c: var LLVMCode; n: Cursor): LLType =
  var nn = n
  result = genTypeLLVM(c, nn)

proc fixedArrayLen(c: var LLVMCode; n: Cursor): int =
  result = -1
  var t = navigateToObjectBody(c.m, n)
  if t.typeKind == ArrayT:
    inc t
    skip t
    if t.kind == IntLit:
      result = int(intVal(t))
    elif t.kind == UIntLit:
      result = int(uintVal(t))

proc typeSizeBits(c: var LLVMCode; n: Cursor): int =
  case n.typeKind
  of IT, UT, CT:
    var nn = n
    inc nn
    if nn.kind == IntLit:
      let bits = intVal(nn)
      result = if bits == -1: c.bits else: int(bits)
    else:
      result = c.bits
  of FT:
    var nn = n
    inc nn
    if nn.kind == IntLit:
      let bits = intVal(nn)
      result = if bits == -1: 64 else: int(bits)
    else:
      result = 64
  of BoolT:
    result = 8
  of PtrT, AptrT, ProctypeT:
    result = c.bits
  of FlexarrayT:
    result = 0
  of EnumT:
    var nn = n
    inc nn
    result = typeSizeBits(c, nn)
  of ArrayT:
    var nn = n
    inc nn
    let elemBits = typeSizeBits(c, nn)
    skip nn
    var count = 0
    if nn.kind == IntLit:
      count = int(intVal(nn))
    elif nn.kind == UIntLit:
      count = int(uintVal(nn))
    result = elemBits * count
  of NoType:
    if n.kind == Symbol:
      let d = c.m.getDeclOrNil(n.symId)
      if d != nil and d.kind == TypeY:
        let decl = asTypeDecl(d.pos)
        result = typeSizeBits(c, decl.body)
      else:
        result = c.bits
    else:
      result = c.bits
  of ObjectT:
    var nn = n
    nn.into:
      if nn.kind == Symbol:
        result = typeSizeBits(c, nn)
        inc nn
      elif nn.kind == DotToken:
        result = 0
        inc nn
      else:
        result = 0
      while nn.hasMore:
        if nn.substructureKind == FldU:
          var fdecl = takeFieldDecl(nn)
          result += typeSizeBits(c, fdecl.typ)
        elif nn.typeKind == UnionT:
          result += typeSizeBits(c, nn)
          skip nn
        else:
          skip nn
  of UnionT:
    var nn = n
    result = 0
    nn.into:
      while nn.hasMore:
        if nn.substructureKind == FldU:
          var fdecl = takeFieldDecl(nn)
          let sz = typeSizeBits(c, fdecl.typ)
          if sz > result: result = sz
        elif nn.typeKind in {ObjectT, UnionT}:
          let sz = typeSizeBits(c, nn)
          if sz > result: result = sz
          skip nn
        else:
          skip nn
  of VoidT, VarargsT, ParamsT:
    result = 0

proc typeAlignBits(c: var LLVMCode; n: Cursor): int =
  case n.typeKind
  of IT, UT, CT, FT, BoolT:
    result = typeSizeBits(c, n)
    if result > c.bits: result = c.bits
  of PtrT, AptrT, ProctypeT:
    result = c.bits
  of EnumT:
    var nn = n
    inc nn
    result = typeAlignBits(c, nn)
  of ArrayT:
    result = typeAlignBits(c, n.firstSon)
  of NoType:
    if n.kind == Symbol:
      let d = c.m.getDeclOrNil(n.symId)
      if d != nil and d.kind == TypeY:
        let decl = asTypeDecl(d.pos)
        result = typeAlignBits(c, decl.body)
      else:
        result = c.bits
    else:
      result = c.bits
  of ObjectT, UnionT:
    var nn = n
    nn.into:
      if n.typeKind == ObjectT:
        if nn.kind == Symbol:
          result = typeAlignBits(c, nn)
          inc nn
        elif nn.kind == DotToken:
          result = 0
          inc nn
        else:
          result = 0
      else:
        result = 0
      while nn.hasMore:
        if nn.substructureKind == FldU:
          var fdecl = takeFieldDecl(nn)
          let a = typeAlignBits(c, fdecl.typ)
          if a > result: result = a
        elif nn.typeKind in {ObjectT, UnionT}:
          let a = typeAlignBits(c, nn)
          if a > result: result = a
          skip nn
        else:
          skip nn
  else:
    result = 8

# ---- Type ordering and definition generation ----

type
  TypeOrderEntryLLVM = (Cursor, bool)

  TypeOrderLLVM = object
    forwardedDecls: seq[Cursor]
    ordered: seq[TypeOrderEntryLLVM]
    lookedAt: IntSet
    lookedAtBodies: HashSet[SymId]

proc traverseObjectBodyLLVM(m: var MainModule; o: var TypeOrderLLVM; t: Cursor)
proc traverseProctypeBodyLLVM(m: var MainModule; o: var TypeOrderLLVM; t: Cursor)

proc recordDependencyImplLLVM(m: var MainModule; o: var TypeOrderLLVM;
                               parent, child: Cursor; viaPointer: var bool) =
  var ch = child
  while true:
    case ch.typeKind
    of AptrT, PtrT:
      viaPointer = true
      ch = elementType(ch)
    of FlexarrayT:
      viaPointer = false
      ch = elementType(ch)
    else:
      break

  case ch.typeKind
  of ObjectT, UnionT:
    let obj = ch
    if viaPointer:
      o.forwardedDecls.add parent
    else:
      if not containsOrIncl(o.lookedAt, obj.toUniqueId()):
        traverseObjectBodyLLVM(m, o, obj)
      o.ordered.add (tracebackTypeC(m, ch), false)
  of ArrayT:
    if viaPointer:
      o.forwardedDecls.add parent
    else:
      if not containsOrIncl(o.lookedAt, ch.toUniqueId()):
        var viaPointer = false
        var elemCur = ch
        inc elemCur
        recordDependencyImplLLVM m, o, ch, elemCur, viaPointer
      o.ordered.add (tracebackTypeC(m, ch), false)
  of EnumT:
    o.ordered.add (tracebackTypeC(m, ch), false)
  of ProctypeT:
    if viaPointer:
      o.forwardedDecls.add parent
    else:
      if not containsOrIncl(o.lookedAt, ch.toUniqueId()):
        traverseProctypeBodyLLVM(m, o, ch)
      o.ordered.add (tracebackTypeC(m, ch), false)
  else:
    if ch.kind == Symbol:
      let id = ch.symId
      let def = m.getDeclOrNil(id)
      if def == nil:
        error m, "undeclared symbol: ", ch
      else:
        var n = def.pos
        let decl = asTypeDecl(n)
        let alreadyFullyProcessed = decl.name.symId in o.lookedAtBodies
        if viaPointer:
          recordDependencyImplLLVM m, o, n, decl.body, viaPointer
        elif not alreadyFullyProcessed:
          o.lookedAtBodies.incl decl.name.symId
          recordDependencyImplLLVM m, o, n, decl.body, viaPointer

proc recordDependencyLLVM(m: var MainModule; o: var TypeOrderLLVM; parent,
    child: Cursor) =
  var viaPointer = false
  recordDependencyImplLLVM m, o, parent, child, viaPointer

proc traverseObjectBodyLLVM(m: var MainModule; o: var TypeOrderLLVM; t: Cursor) =
  let kind = t.typeKind
  var n = t
  n.into:
    if kind == ObjectT:
      if n.kind == Symbol:
        recordDependencyLLVM m, o, t, n
        inc n
      elif n.kind == DotToken:
        inc n
      else:
        error m, "expected `Symbol` or `.` for inheritance but got: ", n
    while n.hasMore:
      if n.substructureKind == FldU:
        let decl = takeFieldDecl(n)
        recordDependencyLLVM m, o, t, decl.typ
      elif n.typeKind in {ObjectT, UnionT}:
        traverseObjectBodyLLVM(m, o, n)
        skip n
      else:
        error m, "unexpected node inside object: ", n

proc traverseProctypeBodyLLVM(m: var MainModule; o: var TypeOrderLLVM; t: Cursor) =
  var n = t
  let procType = takeProcType(n)
  var viaPointer = true
  if procType.params.kind == TagLit:
    var param = procType.params
    param.loopInto:
      let paramDecl = takeParamDecl(param)
      recordDependencyImplLLVM m, o, t, paramDecl.typ, viaPointer
  recordDependencyImplLLVM m, o, t, procType.returnType, viaPointer

proc traverseTypesLLVM(m: var MainModule; o: var TypeOrderLLVM) =
  var i = 0
  while i < m.types.len:
    let n = m.types[i]
    let decl = asTypeDecl(n)
    let t = decl.body
    case t.typeKind
    of ObjectT:
      traverseObjectBodyLLVM m, o, t
      o.ordered.add (n, false)
    of UnionT:
      traverseObjectBodyLLVM m, o, t
      o.ordered.add (n, false)
    of ArrayT:
      recordDependencyLLVM m, o, t, t.firstSon
      o.ordered.add (n, false)
    of ProctypeT:
      traverseProctypeBodyLLVM m, o, t
      o.ordered.add (n, false)
    of EnumT:
      o.ordered.add (n, false)
    else: discard
    inc i
  for fd in o.forwardedDecls:
    var found = false
    for j in 0 ..< o.ordered.len:
      if o.ordered[j][0].toUniqueId() == fd.toUniqueId():
        found = true
        break
    if not found:
      o.ordered.insert((fd, true), 0)

type
  UnionCandidate = object
    typ*: LLType
    sizeBits*: int
    alignBits*: int

proc genObjectBodyLLVM(c: var LLVMCode; n: var Cursor): LLType
proc genUnionBodyLLVM(c: var LLVMCode; n: var Cursor): LLType

proc collectUnionCandidates(c: var LLVMCode; n: Cursor;
    candidates: var seq[UnionCandidate]) =
  var nn = n
  nn.into:
    while nn.hasMore:
      if nn.substructureKind == FldU:
        var decl = takeFieldDecl(nn)
        let sz = typeSizeBits(c, decl.typ)
        let al = typeAlignBits(c, decl.typ)
        var t = decl.typ
        candidates.add UnionCandidate(typ: genTypeLLVM(c, t),
          sizeBits: sz, alignBits: al)
      elif nn.typeKind == ObjectT:
        var obj = nn
        candidates.add UnionCandidate(
          typ: genObjectBodyLLVM(c, obj),
          sizeBits: typeSizeBits(c, nn),
          alignBits: typeAlignBits(c, nn))
        skip nn
      elif nn.typeKind == UnionT:
        var u = nn
        candidates.add UnionCandidate(
          typ: genUnionBodyLLVM(c, u),
          sizeBits: typeSizeBits(c, nn),
          alignBits: typeAlignBits(c, nn))
        skip nn
      else:
        skip nn

proc genUnionBodyLLVM(c: var LLVMCode; n: var Cursor): LLType =
  var candidates: seq[UnionCandidate] = @[]
  collectUnionCandidates(c, n, candidates)
  skip n
  if candidates.len == 0:
    return newLLStructType(@[])
  var maxSize = candidates[0].sizeBits
  var maxAlign = candidates[0].alignBits
  var repIdx = 0
  for i in 1 ..< candidates.len:
    if candidates[i].sizeBits > maxSize: maxSize = candidates[i].sizeBits
    if candidates[i].alignBits > maxAlign: maxAlign = candidates[i].alignBits
    if candidates[i].alignBits > candidates[repIdx].alignBits or
       (candidates[i].alignBits == candidates[repIdx].alignBits and
        candidates[i].sizeBits > candidates[repIdx].sizeBits):
      repIdx = i
  let sizeBytes = (maxSize + 7) div 8
  let alignBytes = maxAlign div 8
  let repSizeBytes = (candidates[repIdx].sizeBits + 7) div 8
  var fields: seq[LLStructField] = @[]
  if alignBytes <= 1:
    fields.add LLStructField(typ: newLLArrayType(sizeBytes, c.prim.i8))
  else:
    fields.add LLStructField(typ: candidates[repIdx].typ)
    let paddingBytes = sizeBytes - repSizeBytes
    if paddingBytes > 0:
      fields.add LLStructField(typ: newLLArrayType(paddingBytes, c.prim.i8))
  result = newLLStructType(fields)

proc flushBitfieldAccum(fields: var seq[LLType]; bitfieldAccum: var int64) =
  if bitfieldAccum > 0:
    var storeBits = 8
    while storeBits < bitfieldAccum: storeBits *= 2
    if storeBits > 64: storeBits = 64
    fields.add newLLIntType(int storeBits)
    bitfieldAccum = 0

proc accumulateBitfield(c: var LLVMCode; fields: var seq[LLType];
                        bitfieldAccum: var int64; bitfieldUnit: var int;
                        bits: int64; typ: Cursor) =
  let unitBits = typeSizeBits(c, typ)
  if bitfieldUnit == 0:
    bitfieldUnit = unitBits
  if unitBits != bitfieldUnit or bitfieldAccum + bits > bitfieldUnit:
    flushBitfieldAccum(fields, bitfieldAccum)
    bitfieldUnit = unitBits
  bitfieldAccum += bits

proc addObjectFieldsLLVM(c: var LLVMCode; n: var Cursor; fields: var seq[LLType];
                         bitfieldAccum: var int64; bitfieldUnit: var int) =
  let kind = n.typeKind
  n.into:
    if kind == ObjectT:
      if n.kind == DotToken:
        inc n
      elif n.kind == Symbol:
        let baseName = mangleSym(c, n.symId)
        fields.add LLType(kind: llStruct, name: baseName)
        inc n
      else:
        error c.m, "expected `Symbol` or `.` for inheritance but got: ", n
    while n.hasMore:
      if n.substructureKind == FldU:
        var decl = takeFieldDecl(n)
        let bits = extractBitfieldBits(decl.pragmas)
        if bits > 0:
          accumulateBitfield(c, fields, bitfieldAccum, bitfieldUnit, bits, decl.typ)
        else:
          flushBitfieldAccum(fields, bitfieldAccum)
          bitfieldUnit = 0
          var t = decl.typ
          fields.add genTypeLLVM(c, t)
      elif n.typeKind == UnionT:
        flushBitfieldAccum(fields, bitfieldAccum)
        bitfieldUnit = 0
        fields.add genUnionBodyLLVM(c, n)
      elif n.typeKind == ObjectT:
        flushBitfieldAccum(fields, bitfieldAccum)
        bitfieldUnit = 0
        addObjectFieldsLLVM(c, n, fields, bitfieldAccum, bitfieldUnit)
      else:
        error c.m, "expected `fld` but got: ", n

proc genObjectBodyLLVM(c: var LLVMCode; n: var Cursor): LLType =
  let kind = n.typeKind
  if kind == UnionT:
    return genUnionBodyLLVM(c, n)
  var fields: seq[LLType] = @[]
  var bitfieldAccum = 0'i64
  var bitfieldUnit = 0
  addObjectFieldsLLVM(c, n, fields, bitfieldAccum, bitfieldUnit)
  flushBitfieldAccum(fields, bitfieldAccum)
  var sfields: seq[LLStructField] = @[]
  for ft in fields:
    sfields.add LLStructField(typ: ft)
  result = newLLStructType(sfields)

type
  FieldAccess* = object
    isBranch*: bool
    index*: int
    branchType*: LLType
    branchIndex*: int

proc searchFieldIdx(c: var LLVMCode; body: var Cursor; fldSym: SymId;
                    access: var FieldAccess; bitfieldAccum: var int64;
                    bitfieldUnit: var int): bool =
  let kind = body.typeKind
  result = false
  body.into:
    if kind == ObjectT:
      if body.kind == Symbol:
        inc body
        access.index = 1
      elif body.kind == DotToken:
        inc body
    while body.hasMore and not result:
      if body.substructureKind == FldU:
        let decl = takeFieldDecl(body)
        let bits = extractBitfieldBits(decl.pragmas)
        if bits > 0:
          let unitBits = typeSizeBits(c, decl.typ)
          if bitfieldUnit == 0:
            bitfieldUnit = unitBits
          if unitBits != bitfieldUnit or bitfieldAccum + bits > bitfieldUnit:
            if bitfieldAccum > 0: inc access.index
            bitfieldAccum = 0
            bitfieldUnit = unitBits
          if decl.name.kind == SymbolDef and decl.name.symId == fldSym:
            return true
          bitfieldAccum += bits
        else:
          if bitfieldAccum > 0:
            inc access.index
            bitfieldAccum = 0
            bitfieldUnit = 0
          if decl.name.kind == SymbolDef and decl.name.symId == fldSym:
            return true
          inc access.index
      elif body.typeKind == UnionT:
        if bitfieldAccum > 0:
          inc access.index
          bitfieldAccum = 0
          bitfieldUnit = 0
        let unionIdx = access.index
        var unionBody = body
        skip unionBody
        var search = body
        inc search
        while search.hasMore:
          if search.substructureKind == FldU:
            let fdecl = takeFieldDecl(search)
            if fdecl.name.kind == SymbolDef and fdecl.name.symId == fldSym:
              var ft = fdecl.typ
              access = FieldAccess(isBranch: true, index: unionIdx,
                  branchType: genUnionBodyLLVM(c, ft),
                  branchIndex: 0)
              return true
          elif search.typeKind == ObjectT:
            var branchCur = search
            let brType = genObjectBodyLLVM(c, branchCur)
            var innerAccess = FieldAccess()
            var brBfAccum = 0'i64
            var brBfUnit = 0
            if searchFieldIdx(c, search, fldSym, innerAccess, brBfAccum, brBfUnit):
              access = FieldAccess(isBranch: true, index: unionIdx,
                  branchType: brType, branchIndex: innerAccess.index)
              return true
          else:
            skip search
        body = unionBody
        inc access.index
      elif body.typeKind == ObjectT:
        if bitfieldAccum > 0:
          inc access.index
          bitfieldAccum = 0
          bitfieldUnit = 0
        if searchFieldIdx(c, body, fldSym, access, bitfieldAccum, bitfieldUnit):
          return true
      else:
        skip body
  return false

proc fieldIndex(c: var LLVMCode; objBody: Cursor; fldSym: SymId): int =
  result = 0
  if objBody.typeKind == UnionT:
    return 0
  var body = objBody
  var bitfieldAccum = 0'i64
  var bitfieldUnit = 0
  var access = FieldAccess()
  discard searchFieldIdx(c, body, fldSym, access, bitfieldAccum, bitfieldUnit)
  result = access.index

proc fieldAccessLLVM(c: var LLVMCode; objBody: Cursor;
    fldSym: SymId): FieldAccess =
  if objBody.typeKind == UnionT:
    result = FieldAccess(index: 0)
    return result
  var body = objBody
  var bitfieldAccum = 0'i64
  var bitfieldUnit = 0
  result = FieldAccess()
  discard searchFieldIdx(c, body, fldSym, result, bitfieldAccum, bitfieldUnit)

proc genTypeDefLLVM(c: var LLVMCode; body: var Cursor; name: string;
                    packed: bool): string =
  ## Generate a named type definition line. Returns "" if nothing to emit.
  case body.typeKind
  of ObjectT, UnionT:
    let structBody = genObjectBodyLLVM(c, body)
    if structBody.structFields.len == 0: return ""
    let ts = serialize(structBody)
    if packed:
      result = "%" & name & " = type <" & ts & ">\n"
    else:
      result = "%" & name & " = type " & ts & "\n"
  of ArrayT:
    body.into:
      let elemType = genTypeLLVM(c, body)
      var sizeStr: string
      if body.kind == IntLit:
        sizeStr = $intVal(body)
      elif body.kind == UIntLit:
        sizeStr = $uintVal(body)
      else:
        sizeStr = "0"
      skip body
      result = "%" & name & " = type [" & sizeStr & " x " &
          serialize(elemType) & "]\n"
      while body.hasMore: skip body
  of EnumT:
    body.into:
      let baseType = genTypeLLVM(c, body)
      while body.hasMore:
        skip body
      result = "%" & name & " = type " & serialize(baseType) & "\n"
  of ProctypeT:
    let procType = takeProcType(body)
    var retType: string
    if procType.returnType.kind == DotToken:
      retType = "void"
    else:
      var rt = procType.returnType
      retType = ""
      serialize(genTypeLLVM(c, rt), retType)
    var paramTypes: seq[string] = @[]
    if procType.params.kind == TagLit:
      var p = procType.params
      p.loopInto:
        let paramDecl = takeParamDecl(p)
        var t = paramDecl.typ
        var ps = ""
        serialize(genTypeLLVM(c, t), ps)
        paramTypes.add ps
    result = "%" & name & " = type " & retType & " (" & paramTypes.join(", ") & ")*\n"
  else:
    result = ""

proc collectStructFieldTypes(c: var LLVMCode; body: var Cursor; types: var seq[LLType];
                             bitfieldAccum: var int64; bitfieldUnit: var int) =
  template flushBf() =
    if bitfieldAccum > 0:
      var storeBits = 8
      while storeBits < bitfieldAccum: storeBits *= 2
      if storeBits > 64: storeBits = 64
      types.add newLLIntType(storeBits)
      bitfieldAccum = 0
      bitfieldUnit = 0
  while body.hasMore:
    if body.substructureKind == FldU:
      var fdecl = takeFieldDecl(body)
      let bits = extractBitfieldBits(fdecl.pragmas)
      if bits > 0:
        let unitBits = typeSizeBits(c, fdecl.typ)
        if bitfieldUnit == 0:
          bitfieldUnit = unitBits
        if unitBits != bitfieldUnit or bitfieldAccum + bits > bitfieldUnit:
          flushBf()
          bitfieldUnit = unitBits
        bitfieldAccum += bits
      else:
        flushBf()
        var t = fdecl.typ
        types.add genTypeLLVM(c, t)
    elif body.typeKind == UnionT:
      flushBf()
      types.add genUnionBodyLLVM(c, body)
    elif body.typeKind == ObjectT:
      flushBf()
      body.into:
        collectStructFieldTypes(c, body, types, bitfieldAccum, bitfieldUnit)
    else:
      inc body
  flushBf()

proc getStructFieldTypes(c: var LLVMCode; typeSym: Cursor): seq[LLType] =
  result = @[]
  if typeSym.kind == Symbol:
    let d = c.m.getDeclOrNil(typeSym.symId)
    if d != nil and d.kind == TypeY:
      let decl = asTypeDecl(d.pos)
      var body = decl.body
      if body.typeKind == UnionT:
        result.add genTypeLLVMReadOnly(c, body)
      elif body.typeKind == ObjectT:
        var bitfieldAccum = 0'i64
        var bitfieldUnit = 0
        body.into:
          if body.kind == Symbol:
            let baseName = mangleToC(c.m.pool.syms[body.symId])
            result.add LLType(kind: llStruct, name: baseName)
            inc body
          elif body.kind == DotToken:
            inc body
          collectStructFieldTypes(c, body, result, bitfieldAccum, bitfieldUnit)

proc isPackedType(c: var LLVMCode; typeSym: Cursor): bool =
  result = false
  if typeSym.kind == Symbol:
    let d = c.m.getDeclOrNil(typeSym.symId)
    if d != nil and d.kind == TypeY:
      let tdecl = asTypeDecl(d.pos)
      if tdecl.pragmas.substructureKind == PragmasU:
        var p = tdecl.pragmas
        p.loopInto:
          if p.pragmaKind == PackedP:
            result = true
          skip p

proc genGlobalConstr(c: var LLVMCode; n: var Cursor;
    declaredType: Cursor): LLValue =
  ## Generate a constant value as an LLValue (kind llvRawText carries the
  ## formatted constant text; typ carries the actual LLVM type).
  template rawConst(t: LLType; v: string): LLValue =
    LLValue(kind: llvRawText, rawText: v, typ: t)
  case n.kind
  of IntLit:
    let typ = genTypeLLVMReadOnly(c, declaredType)
    result = rawConst(typ, $intVal(n))
    inc n
  of UIntLit:
    let typ = genTypeLLVMReadOnly(c, declaredType)
    result = rawConst(typ, $uintVal(n))
    inc n
  of FloatLit:
    let typ = genTypeLLVMReadOnly(c, declaredType)
    result = rawConst(typ, $floatVal(n))
    inc n
  of CharLit:
    let typ = genTypeLLVMReadOnly(c, declaredType)
    result = rawConst(typ, $ord(n.charLit))
    inc n
  of StrLit:
    let s = c.m.pool.strings[n.litId]
    inc n
    if s.len == 0:
      result = rawConst(newLLArrayType(0, c.prim.i8), "zeroinitializer")
    else:
      var escaped = "c\""
      for ch in s:
        let o = ord(ch)
        if o < 32 or o > 126 or ch == '"' or ch == '\\':
          escaped.add '\\'
          escaped.add "0123456789ABCDEF"[o shr 4]
          escaped.add "0123456789ABCDEF"[o and 0xF]
        else:
          escaped.add ch
      escaped.add '"'
      result = rawConst(newLLArrayType(s.len, c.prim.i8), escaped)
  of Symbol:
    let name = mangleSym(c, n.symId)
    c.requestedSyms.incl n.symId
    let typ = genTypeLLVMReadOnly(c, declaredType)
    result = rawConst(typ, "@" & name)
    inc n
  of DotToken:
    let typ = genTypeLLVMReadOnly(c, declaredType)
    result = rawConst(typ, "zeroinitializer")
    inc n
  else:
    case n.exprKind
    of FalseC:
      result = rawConst(c.prim.i8, "0")
      skip n
    of TrueC:
      result = rawConst(c.prim.i8, "1")
      skip n
    of NilC:
      result = rawConst(c.prim.ptrT, "null")
      skip n
    of OconstrC:
      var resultTyp: LLType = nil
      var resultVal: string = ""
      n.into:
        let typeSym = n
        let fieldTypes = getStructFieldTypes(c, n)
        let packed = isPackedType(c, n)
        var fieldNifTypes: seq[Cursor] = @[]
        if n.kind == Symbol:
          let d = c.m.getDeclOrNil(n.symId)
          if d != nil and d.kind == TypeY:
            let decl = asTypeDecl(d.pos)
            var body = decl.body
            if body.typeKind in {ObjectT, UnionT}:
              body.into:
                if decl.body.typeKind == ObjectT:
                  if body.kind == Symbol:
                    fieldNifTypes.add body
                    inc body
                  elif body.kind == DotToken:
                    inc body
                while body.hasMore:
                  if body.substructureKind == FldU:
                    var fdecl = takeFieldDecl(body)
                    fieldNifTypes.add fdecl.typ
                  elif body.typeKind == UnionT:
                    # Traverse into union to collect branch field types
                    # so that branch-specific KvU pairs find the correct NIF type.
                    body.into:
                      while body.hasMore:
                        if body.substructureKind == FldU:
                          var fdecl = takeFieldDecl(body)
                          fieldNifTypes.add fdecl.typ
                        elif body.typeKind in {ObjectT, UnionT}:
                          # Nested object/union inside a union branch.
                          body.into:
                            while body.hasMore:
                              if body.substructureKind == FldU:
                                var fdecl = takeFieldDecl(body)
                                fieldNifTypes.add fdecl.typ
                              else:
                                skip body
                        else:
                          skip body
                  else:
                    skip body
        skip n # type
        var typeParts: seq[LLType] = @[]
        var valParts: seq[string] = @[]
        var usedNifTypes: seq[Cursor] = @[]
        var fieldIdx = 0
        while n.hasMore:
          if n.substructureKind == KvU:
            n.into:
              skip n
              let nifType = if fieldIdx < fieldNifTypes.len: fieldNifTypes[fieldIdx]
                            else: declaredType
              usedNifTypes.add nifType
              let tc = genGlobalConstr(c, n, nifType)
              typeParts.add tc.typ
              valParts.add serialize(tc.typ) & " " & tc.rawText
              while n.hasMore: skip n
            inc fieldIdx
          elif n.exprKind == OconstrC:
            let nifType = if fieldIdx < fieldNifTypes.len: fieldNifTypes[fieldIdx]
                          else: declaredType
            usedNifTypes.add nifType
            let tc = genGlobalConstr(c, n, nifType)
            typeParts.add tc.typ
            valParts.add serialize(tc.typ) & " " & tc.rawText
            inc fieldIdx
          else:
            let nifType = if fieldIdx < fieldNifTypes.len: fieldNifTypes[fieldIdx]
                          else: declaredType
            usedNifTypes.add nifType
            let tc = genGlobalConstr(c, n, nifType)
            typeParts.add tc.typ
            valParts.add serialize(tc.typ) & " " & tc.rawText
            inc fieldIdx
      var needsAnon = false
      for i in 0 ..< min(typeParts.len, fieldTypes.len):
        if not typeEq(typeParts[i], fieldTypes[i]):
          needsAnon = true
          break
      if typeParts.len != fieldTypes.len:
        needsAnon = true
      # For union object types, the branch-specific fields may not fill the
      # full declared type. Add padding to match the declared size.
      if needsAnon:
        let declaredSizeBits = typeSizeBits(c, declaredType)
        var actualSizeBits = 0
        for nt in usedNifTypes:
          actualSizeBits += typeSizeBits(c, nt)
        let paddingBits = declaredSizeBits - actualSizeBits
        if paddingBits > 0:
          let paddingBytes = paddingBits div 8
          let padType = newLLArrayType(paddingBytes, c.prim.i8)
          typeParts.add padType
          valParts.add "[" & $paddingBytes & " x i8] zeroinitializer"
      let valStr = valParts.join(", ")
      if needsAnon:
        var sfields: seq[LLStructField] = @[]
        for ft in typeParts: sfields.add LLStructField(typ: ft)
        let anonTyp = newLLStructType(sfields)
        if packed:
          resultTyp = anonTyp
          resultVal = "<{ " & valStr & " }>"
        else:
          resultTyp = anonTyp
          resultVal = "{ " & valStr & " }"
      else:
        var dt = declaredType
        resultTyp = genTypeLLVM(c, dt)
        if packed:
          resultVal = "<{ " & valStr & " }>"
        else:
          resultVal = "{ " & valStr & " }"
      result = rawConst(resultTyp, resultVal)
    of AconstrC:
      n.into:
        var elemTypeCursor = declaredType
        var elemType: LLType = c.prim.i8
        if declaredType.typeKind == FlexarrayT:
          var et = declaredType
          inc et
          elemType = genTypeLLVMReadOnly(c, et)
          elemTypeCursor = et
        elif declaredType.typeKind == ArrayT:
          var at = declaredType
          inc at
          elemType = genTypeLLVMReadOnly(c, at)
          elemTypeCursor = at
        else:
          if n.typeKind == FlexarrayT:
            var et = n
            inc et
            elemType = genTypeLLVMReadOnly(c, et)
            elemTypeCursor = et
          elif n.typeKind == ArrayT:
            var at = n
            inc at
            elemType = genTypeLLVMReadOnly(c, at)
            elemTypeCursor = at
          elif n.kind == Symbol:
            let d = c.m.getDeclOrNil(n.symId)
            if d != nil and d.kind == TypeY:
              let tdecl = asTypeDecl(d.pos)
              if tdecl.body.typeKind == ArrayT:
                var at = tdecl.body
                inc at
                elemType = genTypeLLVMReadOnly(c, at)
                elemTypeCursor = at
        skip n # type
        var elems: seq[string] = @[]
        while n.hasMore:
          let tc = genGlobalConstr(c, n, elemTypeCursor)
          elems.add serialize(tc.typ) & " " & tc.rawText
        let arrTyp = newLLArrayType(elems.len, elemType)
        result = rawConst(arrTyp, "[ " & elems.join(", ") & " ]")
    of CastC, ConvC:
      n.into:
        skip n
        result = genGlobalConstr(c, n, declaredType)
        while n.hasMore: skip n
    of SufC:
      n.into:
        result = genGlobalConstr(c, n, declaredType)
        skip n
        while n.hasMore: skip n
    of AddrC:
      n.into:
        if n.kind == Symbol:
          let name = mangleSym(c, n.symId)
          c.requestedSyms.incl n.symId
          result = rawConst(c.prim.ptrT, "@" & name)
          inc n
        else:
          error c.m, "unsupported address constant; only plain symbols are currently handled: ", n
        while n.hasMore: skip n
    else:
      error c.m, "unhandled expression in global constant: ", n
