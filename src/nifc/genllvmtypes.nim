#
#
#           NIFC Compiler LLVM Backend
#        (c) Copyright 2024
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvm.nim

## Generates LLVM types from NIFC types.

type
  TypeList = object
    processed: IntSet
    s: seq[(Cursor, string)]

proc add(dest: var TypeList; elem: Cursor; name: string) =
  if not containsOrIncl(dest.processed, elem.toUniqueId()):
    dest.s.add (elem, name)

type
  TypeOrder = object
    forwardedDecls, ordered: TypeList
    lookedAt: IntSet
    lookedAtBodies: HashSet[SymId]

proc traverseObjectBody(m: Module; o: var TypeOrder; t: Cursor)

proc recordDependencyImpl(m: Module; o: var TypeOrder; parent, child: Cursor;
                          viaPointer: var bool) =
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
    let typeName = if ch.typeKind == ObjectT: "struct" else: "union"
    let obj = ch
    if viaPointer:
      o.forwardedDecls.add parent, typeName
    else:
      if not containsOrIncl(o.lookedAt, obj.toUniqueId()):
        traverseObjectBody(m, o, obj)
      o.ordered.add tracebackTypeC(ch), typeName
  of ArrayT:
    if viaPointer:
      o.forwardedDecls.add parent, "struct"
    else:
      if not containsOrIncl(o.lookedAt, ch.toUniqueId()):
         var viaPointer = false
         recordDependencyImpl m, o, ch, ch.firstSon, viaPointer
      o.ordered.add tracebackTypeC(ch), "struct"
  of EnumT:
    # enums do not depend on anything so always safe to generate them
    o.ordered.add tracebackTypeC(ch), "enum"
  else:
    if ch.kind == Symbol:
      # follow the symbol to its definition:
      let id = ch.symId
      let def = m.defs.getOrDefault(id)
      if def.pos == 0:
        if pool.syms[id].endsWith(".c"):
          # imported from c, no need to check dependency
          discard
        else:
          error m, "undeclared symbol: ", ch
      else:
        var n = readonlyCursorAt(m.src, def.pos)
        let decl = asTypeDecl(n)
        if not containsOrIncl(o.lookedAtBodies, decl.name.symId):
          recordDependencyImpl m, o, n, decl.body, viaPointer
    else:
      discard "uninteresting type as we only focus on the required struct declarations"

proc recordDependency(m: Module; o: var TypeOrder; parent, child: Cursor) =
  var viaPointer = false
  recordDependencyImpl m, o, parent, child, viaPointer

proc traverseObjectBody(m: Module; o: var TypeOrder; t: Cursor) =
  var n = t
  inc n
  if n.kind == Symbol:
    # inheritance
    recordDependency m, o, t, n
    inc n
  elif n.kind == DotToken:
    inc n
  else:
    error m, "expected `Symbol` or `.` for inheritance but got: ", n
  while n.substructureKind == FldU:
    let decl = takeFieldDecl(n)
    recordDependency m, o, t, decl.typ

proc traverseProctypeBody(m: Module; o: var TypeOrder; t: Cursor) =
  var n = t
  let procType = takeProcType(n)
  var param = procType.params
  if param.kind == ParLe:
    param = param.firstSon
    while param.kind != ParRi:
      let paramDecl = takeParamDecl(param)
      recordDependency m, o, t, paramDecl.typ
  recordDependency m, o, t, procType.returnType

proc traverseTypes*(m: Module; o: var TypeOrder) =
  for ch in m.types:
    let n = readonlyCursorAt(m.src, ch)
    let decl = asTypeDecl(n)
    let t = decl.body
    case t.typeKind
    of ObjectT:
      traverseObjectBody m, o, t
      o.ordered.add n, "struct"
    of UnionT:
      traverseObjectBody m, o, t
      o.ordered.add n, "union"
    of ArrayT:
      recordDependency m, o, t, t.firstSon
      o.ordered.add n, "struct"
    of ProctypeT:
      traverseProctypeBody m, o, t
      o.ordered.add n, "proctype"
    of EnumT:
      o.ordered.add n, "enum"
    else: discard

proc getLLVMType(c: var GeneratedCode; typeName: string, bits: int = 0): LLVMTypeRef =
  case typeName
  of "void":
    result = LLVMVoidTypeInContext(c.context)
  of "NB8", "bool":
    result = LLVMInt1TypeInContext(c.context)
  of "NI8", "NC8", "int8", "char":
    result = LLVMInt8TypeInContext(c.context)
  of "NI16", "int16":
    result = LLVMInt16TypeInContext(c.context)
  of "NI32", "int32":
    result = LLVMInt32TypeInContext(c.context)
  of "NI64", "int64":
    result = LLVMInt64TypeInContext(c.context)
  of "NU8", "uint8":
    result = LLVMInt8TypeInContext(c.context)
  of "NU16", "uint16":
    result = LLVMInt16TypeInContext(c.context)
  of "NU32", "uint32":
    result = LLVMInt32TypeInContext(c.context)
  of "NU64", "uint64":
    result = LLVMInt64TypeInContext(c.context)
  of "NF32", "float32":
    result = LLVMFloatTypeInContext(c.context)
  of "NF64", "float64", "double":
    result = LLVMDoubleTypeInContext(c.context)
  of "NI", "int":
    if bits > 0:
      result = getLLVMType(c, "int" & $bits)
    else:
      result = LLVMInt32TypeInContext(c.context)
  of "NU", "uint":
    if bits > 0:
      result = getLLVMType(c, "uint" & $bits)
    else:
      result = LLVMInt32TypeInContext(c.context)
  of "NF", "float":
    if bits > 0:
      result = getLLVMType(c, "float" & $bits)
    else:
      result = LLVMFloatTypeInContext(c.context)
  else:
    # Try to find custom type
    if c.types.hasKey(typeName):
      result = c.types[typeName]
    elif c.structs.hasKey(typeName):
      result = c.structs[typeName]
    else:
      # Create opaque struct type for forwards
      result = LLVMStructTypeInContext(c.context, nil, 0, 0.LLVMBool)
      c.structs[typeName] = result

template integralBits(c: GeneratedCode; n: Cursor): int =
  let res = pool.integers[n.intId]
  if res == -1:
    c.intmSize
  else: # 8, 16, 32, 64 etc.
    res

proc genLLVMType(c: var GeneratedCode; n: var Cursor; name = ""): LLVMTypeRef =
  case n.typeKind
  of VoidT:
    result = getLLVMType(c, "void")
    skip n
  of IT:
    inc n
    assert n.kind == IntLit
    let bits = integralBits(c, n)
    inc n
    result = getLLVMType(c, "int", bits)
    while n.kind != ParRi:
      skip n
    skipParRi n
  of UT:
    inc n
    assert n.kind == IntLit
    let bits = integralBits(c, n)
    inc n
    result = getLLVMType(c, "uint", bits)
    while n.kind != ParRi:
      skip n
    skipParRi n
  of FT:
    inc n
    assert n.kind == IntLit
    let bits = integralBits(c, n)
    inc n
    result = getLLVMType(c, "float", bits)
    while n.kind != ParRi:
      skip n
    skipParRi n
  of BoolT:
    inc n
    result = getLLVMType(c, "bool")
    while n.kind != ParRi:
      skip n
    inc n
  of CT:
    inc n
    assert n.kind == IntLit
    let bits = integralBits(c, n)
    inc n
    result = getLLVMType(c, "char", bits)
    while n.kind != ParRi:
      skip n
    skipParRi n
  of NoType:
    if n.kind == Symbol:
      let typeName = mangle(pool.syms[n.symId])
      result = getLLVMType(c, typeName)
      inc n
    else:
      error c.m, "node is not a type: ", n
  of PtrT, APtrT:
    inc n
    let elemType = genLLVMType(c, n)
    result = LLVMPointerType(elemType, 0)
    while n.kind != ParRi:
      skip n
    inc n
  of FlexarrayT:
    inc n
    let elemType = genLLVMType(c, n)
    # Create a zero-length array type to represent flexarray
    result = LLVMArrayType(elemType, 0)
    skipParRi n
  of ProctypeT:
    var procType = takeProcType(n)
    var paramTypes: seq[LLVMTypeRef] = @[]
    var isVarArgs = false

    # Check pragmas for varargs
    if procType.pragmas.kind == ParLe:
      var pragmas = procType.pragmas
      inc pragmas
      while pragmas.kind != ParRi:
        if pragmas.pragmaKind == VarargsP:
          isVarArgs = true
        skip pragmas

    # Get param types
    if procType.params.kind == ParLe:
      var param = procType.params.firstSon
      while param.kind != ParRi:
        var paramDecl = takeParamDecl(param)
        paramTypes.add genLLVMType(c, paramDecl.typ)

    # Get return type
    let returnType = if procType.returnType.kind == DotToken:
      LLVMVoidTypeInContext(c.context)
    else:
      genLLVMType(c, procType.returnType)

    # Create function type
    var paramTypeArray = default(array[32, LLVMTypeRef])
    for i in 0..<min(paramTypes.len, 32):
      paramTypeArray[i] = paramTypes[i]

    result = LLVMFunctionType(
      returnType,
      if paramTypes.len > 0: addr paramTypeArray[0] else: nil,
      paramTypes.len.cuint,
      (if isVarArgs: 1 else: 0).LLVMBool
    )

  of ArrayT, ParamsT, UnionT, ObjectT, EnumT:
    error c.m, "nominal type not allowed here: ", n

proc generateForwardStructDecl(c: var GeneratedCode; name: string) =
  if not c.structs.hasKey(name):
    let structType = LLVMStructTypeInContext(c.context, nil, 0, 0.LLVMBool)
    c.structs[name] = structType

proc genFieldTypes(c: var GeneratedCode; n: var Cursor, fields: var seq[LLVMTypeRef]) =
  inc n
  if n.kind == Symbol:
    # Handle inheritance if needed
    inc n
  elif n.kind == DotToken:
    inc n
  else:
    error c.m, "expected `Symbol` or `.` for inheritance but got: ", n

  while n.kind != ParRi:
    if n.substructureKind == FldU:
      var decl = takeFieldDecl(n)
      var bits: BiggestInt = 0
      if decl.pragmas.kind != DotToken:
        var pragmas = decl.pragmas
        inc pragmas
        while pragmas.kind != ParRi:
          if pragmas.pragmaKind == BitsP:
            inc pragmas
            bits = pool.integers[pragmas.intId]
            skip pragmas
            skipParRi pragmas
          else:
            skip pragmas
        inc pragmas

      let fieldType = genLLVMType(c, decl.typ)
      fields.add fieldType
    else:
      error c.m, "expected `fld` but got: ", n
  inc n # ParRi

proc genEnumFields(c: var GeneratedCode; n: var Cursor; name: string) =
  inc n
  # Base type
  let baseType = genLLVMType(c, n)
  c.types[name] = baseType

  # Process enum fields
  while n.kind != ParRi:
    if n.substructureKind == EfldU:
      inc n
      if n.kind == SymbolDef:
        let enumFieldName = mangle(pool.syms[n.symId])
        inc n

        # Create global constant for enum value
        var value: LLVMValueRef
        case n.kind
        of IntLit:
          value = LLVMConstInt(baseType, pool.integers[n.intId].culonglong, 1.LLVMBool)
          inc n
        of UIntLit:
          value = LLVMConstInt(baseType, pool.uintegers[n.uintId].culonglong, 0.LLVMBool)
          inc n
        else:
          error c.m, "expected `Number` but got: ", n

        # Store value in globals table
        c.globals[enumFieldName] = value
      else:
        error c.m, "expected `SymbolDef` but got: ", n
      skipParRi n
    else:
      error c.m, "expected `efld` but got: ", n
  inc n # ParRi

proc genObjectType(c: var GeneratedCode; n: var Cursor; name: string; isUnion: bool) =
  var fields: seq[LLVMTypeRef] = @[]

  # Get field types
  genFieldTypes(c, n, fields)

  # Create struct type
  var fieldTypeArray: array[128, LLVMTypeRef] # Assuming max 128 fields for simplicity
  for i in 0..<min(fields.len, 128):
    fieldTypeArray[i] = fields[i]

  let structType = if c.structs.hasKey(name):
    c.structs[name]
  else:
    LLVMStructTypeInContext(c.context, nil, 0, 0.LLVMBool)

  # If this is an opaque type, fill it in
  if fields.len > 0:
    # Note: Currently no distinction between union and struct in LLVM IR
    # For unions, we'd need manual bitcasting between different field types
    LLVMStructSetBody(structType,
      if fields.len > 0: addr fieldTypeArray[0] else: nil,
      fields.len.cuint,
      0.LLVMBool)

  c.structs[name] = structType
  c.types[name] = structType

proc genArrayType(c: var GeneratedCode; n: var Cursor; name: string) =
  inc n
  var elemType = genLLVMType(c, n)
  var size: cuint = 0

  var elem = n.firstSon
  skip elem # Skip element type
  case elem.kind
  of IntLit:
    size = pool.integers[elem.intId].cuint
  of UIntLit:
    size = pool.uintegers[elem.uintId].cuint
  else:
    error c.m, "array size must be an int literal, but got: ", elem

  let arrayType = LLVMArrayType(elemType, size)
  c.types[name] = arrayType

proc generateTypes*(c: var GeneratedCode; o: TypeOrder) =
  # Generate forward declarations
  for (d, typeKind) in o.forwardedDecls.s:
    var n = d
    let decl = takeTypeDecl(n)
    let name = mangle(pool.syms[decl.name.symId])
    generateForwardStructDecl(c, name)

  # Generate full type definitions
  for (d, typeKind) in o.ordered.s:
    var n = d
    var decl = takeTypeDecl(n)
    let name = mangle(pool.syms[decl.name.symId])

    if not c.generatedTypes.containsOrIncl(decl.name.symId):
      case decl.body.typeKind
      of ArrayT:
        genArrayType(c, decl.body, name)
      of EnumT:
        genEnumFields(c, decl.body, name)
      of ProctypeT:
        let fnType = genLLVMType(c, decl.body)
        c.types[name] = fnType
      of ObjectT:
        genObjectType(c, decl.body, name, false)
      of UnionT:
        genObjectType(c, decl.body, name, true)
      else:
        error c.m, "unexpected type kind: ", decl.body