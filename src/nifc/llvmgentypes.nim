#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvmcodegen.nim
# Generates LLVM IR types from NIFC types.

proc integralBitsLLVM(t: Cursor; c: LLVMCode): string {.inline.} =
  let res = pool.integers[t.intId]
  if res == -1:
    result = $c.bits
  else:
    result = $res

proc genTypeLLVM(c: var LLVMCode; n: var Cursor): string =
  ## Returns the LLVM IR type string for a NIF type node.
  case n.typeKind
  of VoidT:
    result = "void"
    skip n
  of IT:
    inc n
    if n.kind == IntLit:
      result = "i" & integralBitsLLVM(n, c)
      inc n
    else:
      result = "i" & $c.bits
    # consume qualifiers
    while n.kind != ParRi:
      skip n
    skipParRi n
  of UT:
    inc n
    if n.kind == IntLit:
      result = "i" & integralBitsLLVM(n, c)
      inc n
    else:
      result = "i" & $c.bits
    while n.kind != ParRi:
      skip n
    skipParRi n
  of FT:
    inc n
    var bits = "64"
    if n.kind == IntLit:
      bits = integralBitsLLVM(n, c)
      inc n
    while n.kind != ParRi:
      skip n
    skipParRi n
    case bits
    of "32": result = "float"
    of "64": result = "double"
    of "128": result = "fp128"
    else: result = "double"
  of BoolT:
    inc n
    while n.kind != ParRi:
      skip n
    skipParRi n
    result = "i8" # Use i8 for bool to match C ABI (i1 has different ABI)
  of CT:
    inc n
    if n.kind == IntLit:
      result = "i" & integralBitsLLVM(n, c)
      inc n
    else:
      result = "i8"
    while n.kind != ParRi:
      skip n
    skipParRi n
  of NoType:
    if n.kind == Symbol:
      # Ensure the type definition is loaded for cross-module types
      let d = c.m.getDeclOrNil(n.symId)
      if d != nil and d.kind == TypeY:
        let decl = asTypeDecl(d.pos)
        if decl.body.typeKind == ProctypeT:
          # Function pointer types are just ptr in opaque pointer mode
          result = "ptr"
          inc n
          return
      # Use mangleToC for type references to match type definitions
      let name = mangleToC(pool.syms[n.symId])
      result = "%" & name
      inc n
    elif n.kind == DotToken:
      result = "void"
      inc n
    else:
      error c.m, "node is not a type: ", n
  of PtrT, APtrT:
    result = "ptr"
    skip n
  of FlexarrayT:
    inc n
    let elemType = genTypeLLVM(c, n)
    result = "[0 x " & elemType & "]"
    skipParRi n
  of ProctypeT:
    # Function pointers are just ptr in LLVM's opaque pointer world
    result = "ptr"
    skip n
  of VarargsT:
    result = "..." # special case, handled in function signatures
    skip n
  of EnumT:
    # Enum type encountered inline - use its underlying type
    inc n
    result = genTypeLLVM(c, n) # base type
    while n.kind != ParRi:
      skip n
    skipParRi n
  of ArrayT:
    inc n
    let elemType = genTypeLLVM(c, n)
    var sizeStr = "0"
    if n.kind == IntLit:
      sizeStr = $pool.integers[n.intId]
      skip n
    elif n.kind == UIntLit:
      sizeStr = $pool.uintegers[n.uintId]
      skip n
    skipParRi n
    result = "[" & sizeStr & " x " & elemType & "]"
  of ObjectT, UnionT:
    let typeDecl = tracebackTypeC(n)
    let decl = asTypeDecl(typeDecl)
    if decl.name.kind == SymbolDef:
      discard c.m.getDeclOrNil(decl.name.symId)
      result = "%" & mangleToC(pool.syms[decl.name.symId])
    else:
      result = "ptr"
    skip n
  of ParamsT:
    error c.m, "params type not allowed in expression context: ", n

proc genTypeLLVMReadOnly(c: var LLVMCode; n: Cursor): string =
  ## Same as genTypeLLVM but doesn't advance the cursor.
  var nn = n
  result = genTypeLLVM(c, nn)

proc typeSizeBits(c: var LLVMCode; n: Cursor): int =
  ## Estimate the size of a type in bits for union layout computation.
  case n.typeKind
  of IT, UT, CT:
    var nn = n
    inc nn
    if nn.kind == IntLit:
      let bits = pool.integers[nn.intId]
      result = if bits == -1: c.bits else: int(bits)
    else:
      result = c.bits
  of FT:
    var nn = n
    inc nn
    if nn.kind == IntLit:
      let bits = pool.integers[nn.intId]
      result = if bits == -1: 64 else: int(bits)
    else:
      result = 64
  of BoolT:
    result = 8
  of PtrT, APtrT, ProctypeT:
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
      count = int(pool.integers[nn.intId])
    elif nn.kind == UIntLit:
      count = int(pool.uintegers[nn.uintId])
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
    # Sum of all field sizes (simplified, ignores padding)
    var nn = n
    inc nn
    if nn.kind == Symbol:
      result = typeSizeBits(c, nn)
      inc nn
    elif nn.kind == DotToken:
      result = 0
      inc nn
    else:
      result = 0
    while nn.kind != ParRi:
      if nn.substructureKind == FldU:
        var fdecl = takeFieldDecl(nn)
        result += typeSizeBits(c, fdecl.typ)
      else:
        skip nn
  of UnionT:
    # Max of all field sizes
    var nn = n
    inc nn
    result = 0
    while nn.kind != ParRi:
      if nn.substructureKind == FldU:
        var fdecl = takeFieldDecl(nn)
        let sz = typeSizeBits(c, fdecl.typ)
        if sz > result: result = sz
      else:
        skip nn
  of VoidT, VarargsT, ParamsT:
    result = 0

proc typeAlignBits(c: var LLVMCode; n: Cursor): int =
  ## Estimate the alignment of a type in bits for union layout.
  case n.typeKind
  of IT, UT, CT, FT, BoolT:
    result = typeSizeBits(c, n)
    if result > c.bits: result = c.bits
  of PtrT, APtrT, ProctypeT:
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
    inc nn
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
    while nn.kind != ParRi:
      if nn.substructureKind == FldU:
        var fdecl = takeFieldDecl(nn)
        let a = typeAlignBits(c, fdecl.typ)
        if a > result: result = a
      else:
        skip nn
  else:
    result = 8

# ---- Type ordering and definition generation ----

type
  TypeOrderEntryLLVM = (Cursor, bool) # (cursor, isForward)

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
    of APtrT, PtrT:
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
      o.ordered.add (tracebackTypeC(ch), false)
  of ArrayT:
    if viaPointer:
      o.forwardedDecls.add parent
    else:
      if not containsOrIncl(o.lookedAt, ch.toUniqueId()):
        var viaPointer = false
        recordDependencyImplLLVM m, o, ch, ch.firstSon, viaPointer
      o.ordered.add (tracebackTypeC(ch), false)
  of EnumT:
    o.ordered.add (tracebackTypeC(ch), false)
  of ProctypeT:
    if viaPointer:
      o.forwardedDecls.add parent
    else:
      if not containsOrIncl(o.lookedAt, ch.toUniqueId()):
        traverseProctypeBodyLLVM(m, o, ch)
      o.ordered.add (tracebackTypeC(ch), false)
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

proc recordDependencyLLVM(m: var MainModule; o: var TypeOrderLLVM; parent, child: Cursor) =
  var viaPointer = false
  recordDependencyImplLLVM m, o, parent, child, viaPointer

proc traverseObjectBodyLLVM(m: var MainModule; o: var TypeOrderLLVM; t: Cursor) =
  let kind = t.typeKind
  var n = t
  inc n
  if kind == ObjectT:
    if n.kind == Symbol:
      recordDependencyLLVM m, o, t, n
      inc n
    elif n.kind == DotToken:
      inc n
    else:
      error m, "expected `Symbol` or `.` for inheritance but got: ", n
  var nested = 1
  while true:
    case n.kind:
    of ParRi:
      dec nested
      inc n
      if nested == 0: break
    of ParLe:
      if n.substructureKind == FldU:
        let decl = takeFieldDecl(n)
        recordDependencyLLVM m, o, t, decl.typ
      elif n.typeKind in {ObjectT, UnionT}:
        inc nested
        if n.typeKind == ObjectT:
          inc n
          inc n # base
        else:
          inc n
      else:
        error m, "unexpected node inside object: ", n
    else:
      error m, "unexpected token inside object: ", n

proc traverseProctypeBodyLLVM(m: var MainModule; o: var TypeOrderLLVM; t: Cursor) =
  var n = t
  let procType = takeProcType(n)
  var param = procType.params
  var viaPointer = true
  if param.kind == ParLe:
    param = param.firstSon
    while param.kind != ParRi:
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
  # Mark forward declarations
  for fd in o.forwardedDecls:
    var found = false
    for j in 0 ..< o.ordered.len:
      if o.ordered[j][0].toUniqueId() == fd.toUniqueId():
        found = true
        break
    if not found:
      o.ordered.insert((fd, true), 0)

proc genUnionBodyLLVM(c: var LLVMCode; n: var Cursor): string =
  ## Generate LLVM type for a union: a byte array sized to the largest member,
  ## with alignment matching the most-aligned member.
  inc n # skip UnionT tag
  var maxSizeBits = 0
  var maxAlignBits = 8
  var nested = 1
  while true:
    case n.kind
    of ParRi:
      dec nested
      inc n
      if nested == 0: break
    of ParLe:
      if n.substructureKind == FldU:
        var decl = takeFieldDecl(n)
        let sz = typeSizeBits(c, decl.typ)
        let al = typeAlignBits(c, decl.typ)
        if sz > maxSizeBits: maxSizeBits = sz
        if al > maxAlignBits: maxAlignBits = al
      elif n.typeKind in {ObjectT, UnionT}:
        inc nested
        if n.typeKind == ObjectT:
          inc n
          inc n # base
        else:
          inc n
      else:
        skip n
    else:
      skip n
  let sizeBytes = (maxSizeBits + 7) div 8
  let alignBytes = maxAlignBits div 8
  if sizeBytes == 0:
    result = "{ }"
  elif alignBytes <= 1:
    result = "{ [" & $sizeBytes & " x i8] }"
  else:
    # Use an aligned integer as first element to force alignment,
    # then fill the remainder with i8.
    let alignType = "i" & $maxAlignBits
    let alignTypeBytes = alignBytes
    let remainBytes = sizeBytes - alignTypeBytes
    if remainBytes <= 0:
      result = "{ " & alignType & " }"
    else:
      result = "{ " & alignType & ", [" & $remainBytes & " x i8] }"

proc flushBitfieldAccum(fields: var seq[string]; bitfieldAccum: var int64) =
  ## Emit the accumulated bitfield group as an integer field and reset.
  if bitfieldAccum > 0:
    var storeBits = 8
    while storeBits < bitfieldAccum: storeBits *= 2
    if storeBits > 64: storeBits = 64
    fields.add "i" & $storeBits
    bitfieldAccum = 0

proc accumulateBitfield(c: var LLVMCode; fields: var seq[string];
                        bitfieldAccum: var int64; bitfieldUnit: var int;
                        bits: int64; typ: Cursor) =
  ## Add `bits` to the current bitfield group. If they don't fit into the
  ## current storage unit (determined by the field's declared type), flush
  ## the current group and start a new one.
  let unitBits = typeSizeBits(c, typ)
  if bitfieldUnit == 0:
    # First bitfield in a group — the declared type sets the unit size
    bitfieldUnit = unitBits
  if unitBits != bitfieldUnit or bitfieldAccum + bits > bitfieldUnit:
    # Doesn't fit or different storage unit type — flush and start fresh
    flushBitfieldAccum(fields, bitfieldAccum)
    bitfieldUnit = unitBits
  bitfieldAccum += bits

proc genObjectBodyLLVM(c: var LLVMCode; n: var Cursor): string =
  ## Generate the fields of an object as an LLVM struct body.
  ## For unions, delegates to genUnionBodyLLVM.
  let kind = n.typeKind
  if kind == UnionT:
    return genUnionBodyLLVM(c, n)

  inc n
  var fields: seq[string] = @[]
  var bitfieldAccum = 0'i64
  var bitfieldUnit = 0 # size of the current bitfield storage unit in bits

  if kind == ObjectT:
    if n.kind == DotToken:
      inc n
    elif n.kind == Symbol:
      let baseName = mangleSym(c, n.symId)
      fields.add "%" & baseName
      inc n
    else:
      error c.m, "expected `Symbol` or `.` for inheritance but got: ", n

  var nested = 1
  while true:
    case n.kind:
    of ParRi:
      dec nested
      inc n
      if nested == 0: break
    of ParLe:
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
      elif n.typeKind == ObjectT:
        flushBitfieldAccum(fields, bitfieldAccum)
        bitfieldUnit = 0
        inc nested
        inc n
        inc n # base (must be DotToken for anonymous)
      elif n.typeKind == UnionT:
        flushBitfieldAccum(fields, bitfieldAccum)
        bitfieldUnit = 0
        var unionCur = n
        fields.add genUnionBodyLLVM(c, unionCur)
        inc nested
        inc n
      else:
        error c.m, "expected `fld` but got: ", n
    else:
      error c.m, "expected `fld` but got: ", n

  flushBitfieldAccum(fields, bitfieldAccum)
  result = "{ " & fields.join(", ") & " }"

proc fieldIndex(c: var LLVMCode; objBody: Cursor; fldSym: SymId): int =
  ## Look up the LLVM struct field index for `fldSym` in object body `objBody`.
  ## Accounts for bitfield packing (multiple bitfields may share one storage
  ## unit, or overflow into the next) and union layout (always index 0).
  result = 0
  if objBody.typeKind == UnionT:
    return 0

  if objBody.typeKind == ObjectT:
    var body = objBody
    inc body
    if body.kind == Symbol:
      inc body
      result = 1 # base type occupies field 0
    elif body.kind == DotToken:
      inc body

    var bitfieldAccum = 0'i64
    var bitfieldUnit = 0
    var nested = 1
    while nested > 0:
      case body.kind
      of ParRi:
        dec nested
        inc body
      of ParLe:
        if body.substructureKind == FldU:
          let decl = takeFieldDecl(body)
          let bits = extractBitfieldBits(decl.pragmas)
          if bits > 0:
            let unitBits = typeSizeBits(c, decl.typ)
            if bitfieldUnit == 0:
              bitfieldUnit = unitBits
            if unitBits != bitfieldUnit or bitfieldAccum + bits > bitfieldUnit:
              # Overflow: flush current group, start new one
              if bitfieldAccum > 0: inc result
              bitfieldAccum = 0
              bitfieldUnit = unitBits
            if decl.name.kind == SymbolDef and decl.name.symId == fldSym:
              return
            bitfieldAccum += bits
          else:
            if bitfieldAccum > 0:
              inc result
              bitfieldAccum = 0
              bitfieldUnit = 0
            if decl.name.kind == SymbolDef and decl.name.symId == fldSym:
              return
            inc result
        elif body.typeKind == UnionT:
          if bitfieldAccum > 0:
            inc result
            bitfieldAccum = 0
            bitfieldUnit = 0
          # The union is one LLVM field. Check if fldSym is inside it.
          let unionIdx = result
          var unionBody = body
          skip unionBody # skip past the entire union
          # Search for fldSym inside the union
          var search = body
          inc search # skip UnionT tag
          while search.kind != ParRi:
            if search.substructureKind == FldU:
              let fdecl = takeFieldDecl(search)
              if fdecl.name.kind == SymbolDef and fdecl.name.symId == fldSym:
                return unionIdx
            elif search.typeKind == ObjectT:
              inc search # skip ObjectT
              inc search # skip base
            else:
              skip search
          body = unionBody
          inc result
        elif body.typeKind == ObjectT:
          if bitfieldAccum > 0:
            inc result
            bitfieldAccum = 0
            bitfieldUnit = 0
          inc nested
          inc body
          inc body # skip base
        else:
          skip body
      else:
        inc body

proc genTypeDefLLVM(c: var LLVMCode; body: var Cursor; name: string;
                    packed: bool): string =
  ## Generate a named type definition. Returns the full definition line.
  case body.typeKind
  of ObjectT, UnionT:
    let structBody = genObjectBodyLLVM(c, body)
    if packed:
      result = "%" & name & " = type <" & structBody & ">\n"
    else:
      result = "%" & name & " = type " & structBody & "\n"
  of ArrayT:
    inc body
    let elemType = genTypeLLVM(c, body)
    # body now points to the array size
    var sizeStr: string
    if body.kind == IntLit:
      sizeStr = $pool.integers[body.intId]
    elif body.kind == UIntLit:
      sizeStr = $pool.uintegers[body.uintId]
    else:
      sizeStr = "0"
    skip body
    skipParRi body
    result = "%" & name & " = type [" & sizeStr & " x " & elemType & "]\n"
  of EnumT:
    # Enums are just their underlying integer type, no struct wrapper
    inc body
    let baseType = genTypeLLVM(c, body)
    while body.kind != ParRi:
      skip body
    inc body
    result = "%" & name & " = type " & baseType & "\n"
  of ProctypeT:
    # Function types - generate the actual function type
    let procType = takeProcType(body)
    var retType: string
    if procType.returnType.kind == DotToken:
      retType = "void"
    else:
      var rt = procType.returnType
      retType = genTypeLLVM(c, rt)
    var paramTypes: seq[string] = @[]
    if procType.params.kind == ParLe:
      var p = procType.params.firstSon
      while p.kind != ParRi:
        let paramDecl = takeParamDecl(p)
        var t = paramDecl.typ
        paramTypes.add genTypeLLVM(c, t)
    result = "%" & name & " = type " & retType & " (" & paramTypes.join(", ") & ")*\n"
  else:
    result = ""

proc getStructFieldTypes(c: var LLVMCode; typeSym: Cursor): seq[LToken] =
  ## Given a type symbol cursor, resolve the LLVM struct field types as LTokens.
  ## Accounts for bitfield grouping and union layout.
  result = @[]
  if typeSym.kind == Symbol:
    let d = c.m.getDeclOrNil(typeSym.symId)
    if d != nil and d.kind == TypeY:
      let decl = asTypeDecl(d.pos)
      var body = decl.body
      if body.typeKind == UnionT:
        # Union is a single byte-array element; constant init uses the whole union type
        let unionBody = genTypeLLVMReadOnly(c, body)
        result.add c.tok(unionBody)
      elif body.typeKind == ObjectT:
        inc body
        if body.kind == Symbol:
          let baseName = mangleToC(pool.syms[body.symId])
          result.add c.tok("%" & baseName)
          inc body
        elif body.kind == DotToken:
          inc body
        var bitfieldAccum = 0'i64
        var bitfieldUnit = 0
        var nested = 1
        template flushBf() =
          if bitfieldAccum > 0:
            var storeBits = 8
            while storeBits < bitfieldAccum: storeBits *= 2
            if storeBits > 64: storeBits = 64
            result.add c.tok("i" & $storeBits)
            bitfieldAccum = 0
            bitfieldUnit = 0
        while true:
          case body.kind
          of ParRi:
            dec nested
            inc body
            if nested == 0: break
          of ParLe:
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
                result.add c.tok(genTypeLLVM(c, t))
            elif body.typeKind in {ObjectT, UnionT}:
              flushBf()
              inc nested
              if body.typeKind == ObjectT:
                inc body
                inc body # base
              else:
                inc body
            else:
              inc body
          else:
            inc body
        flushBf()

proc isPackedType(c: var LLVMCode; typeSym: Cursor): bool =
  ## Check if a type symbol refers to a packed struct.
  result = false
  if typeSym.kind == Symbol:
    let d = c.m.getDeclOrNil(typeSym.symId)
    if d != nil and d.kind == TypeY:
      let tdecl = asTypeDecl(d.pos)
      if tdecl.pragmas.substructureKind == PragmasU:
        var p = tdecl.pragmas.firstSon
        while p.kind != ParRi:
          if p.pragmaKind == PackedP: return true
          skip p

proc genConstantLLVM(c: var LLVMCode; n: var Cursor; expectedType: string): string =
  ## Generate a constant initializer for global variables.
  case n.kind
  of IntLit:
    result = $pool.integers[n.intId]
    inc n
  of UIntLit:
    result = $pool.uintegers[n.uintId]
    inc n
  of FloatLit:
    let f = pool.floats[n.floatId]
    # LLVM requires hex notation for floating point constants to avoid precision loss
    result = $f
    inc n
  of CharLit:
    result = $ord(n.charLit)
    inc n
  of StringLit:
    # For string constants in global context, we need to create a global string
    # and return a pointer to it
    result = "zeroinitializer" # placeholder - string globals are complex
    inc n
  of DotToken:
    result = "zeroinitializer"
    inc n
  else:
    case n.exprKind
    of FalseC:
      result = "0"
      skip n
    of TrueC:
      result = "1"
      skip n
    of NilC:
      result = "null"
      skip n
    of OconstrC:
      inc n
      let fieldTypes = getStructFieldTypes(c, n)
      let packed = isPackedType(c, n)
      skip n # type
      var fields: seq[string] = @[]
      var fieldIdx = 0
      while n.kind != ParRi:
        if n.substructureKind == KvU:
          inc n
          skip n # field name
          var ftyp = c.str(fieldTypes[fieldIdx])
          # For flexarray fields with aconstr values, fix the element count
          if n.exprKind == AconstrC:
            var acType = n
            inc acType # skip AconstrC tag
            if acType.typeKind == FlexarrayT:
              var elemTypeCur = acType.firstSon
              let elemTypStr = genTypeLLVMReadOnly(c, elemTypeCur)
              var cnt = 0
              var elem = acType
              skip elem # skip flexarray type
              while elem.kind != ParRi:
                inc cnt
                skip elem
              ftyp = "[" & $cnt & " x " & elemTypStr & "]"
          let val = genConstantLLVM(c, n, "")
          fields.add ftyp & " " & val
          if n.kind != ParRi: skip n # optional inheritance depth
          skipParRi n
          inc fieldIdx
        elif n.exprKind == OconstrC:
          let val = genConstantLLVM(c, n, "")
          fields.add c.str(fieldTypes[fieldIdx]) & " " & val
          inc fieldIdx
        else:
          let val = genConstantLLVM(c, n, "")
          fields.add c.str(fieldTypes[fieldIdx]) & " " & val
          inc fieldIdx
      skipParRi n
      if packed:
        result = "<{ " & fields.join(", ") & " }>"
      else:
        result = "{ " & fields.join(", ") & " }"
    of AconstrC:
      inc n
      var elemType = ""
      # Extract element type from array type
      if n.kind == Symbol:
        let d = c.m.getDeclOrNil(n.symId)
        if d != nil and d.kind == TypeY:
          let tdecl = asTypeDecl(d.pos)
          if tdecl.body.typeKind == ArrayT:
            var at = tdecl.body
            inc at
            elemType = genTypeLLVMReadOnly(c, at)
      elif n.typeKind == ArrayT:
        var at = n
        inc at
        elemType = genTypeLLVMReadOnly(c, at)
      elif n.typeKind == FlexarrayT:
        elemType = genTypeLLVMReadOnly(c, n.firstSon)
      skip n # type
      var elems: seq[string] = @[]
      while n.kind != ParRi:
        let val = genConstantLLVM(c, n, "")
        if elemType.len > 0:
          elems.add elemType & " " & val
        else:
          elems.add val
      skipParRi n
      result = "[ " & elems.join(", ") & " ]"
    of CastC, ConvC:
      inc n
      skip n # type
      result = genConstantLLVM(c, n, expectedType)
      skipParRi n
    of SufC:
      inc n
      result = genConstantLLVM(c, n, "")
      skip n # suffix
      skipParRi n
    else:
      result = "zeroinitializer"
      skip n
