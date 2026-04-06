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
      let name = mangleSym(c, n.symId)
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
    result = "{ [" & sizeStr & " x " & elemType & "] }"
  of ObjectT, UnionT:
    # Anonymous struct/union inline - return as ptr for simplicity
    result = "ptr"
    skip n
  of ParamsT:
    error c.m, "params type not allowed in expression context: ", n

proc genTypeLLVMReadOnly(c: var LLVMCode; n: Cursor): string =
  ## Same as genTypeLLVM but doesn't advance the cursor.
  var nn = n
  result = genTypeLLVM(c, nn)

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

proc genObjectBodyLLVM(c: var LLVMCode; n: var Cursor): string =
  ## Generate the fields of an object/union as an LLVM struct body.
  let kind = n.typeKind
  inc n
  var fields: seq[string] = @[]

  if kind == ObjectT:
    if n.kind == DotToken:
      inc n
    elif n.kind == Symbol:
      # inheritance - include base type as first field
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
        # Skip pragma processing for field type extraction
        var t = decl.typ
        let fieldType = genTypeLLVM(c, t)
        fields.add fieldType
      elif n.typeKind == ObjectT:
        # Anonymous nested object - collect its fields into a sub-struct
        inc nested
        inc n
        inc n # base (must be DotToken for anonymous)
      elif n.typeKind == UnionT:
        # Anonymous union - we'll approximate with the largest field
        inc nested
        inc n
      else:
        error c.m, "expected `fld` but got: ", n
    else:
      error c.m, "expected `fld` but got: ", n

  if kind == UnionT and fields.len > 0:
    # For unions, use a byte array of the maximum size
    # This is a simplification; ideally we'd track sizes properly
    # For now, just use the first field (approximation)
    result = "{ " & fields[0] & " }"
  else:
    result = "{ " & fields.join(", ") & " }"

proc genTypeDefLLVM(c: var LLVMCode; body: var Cursor; name: string): string =
  ## Generate a named type definition. Returns the full definition line.
  case body.typeKind
  of ObjectT, UnionT:
    let structBody = genObjectBodyLLVM(c, body)
    result = "%" & name & " = type " & structBody & "\n"
  of ArrayT:
    inc body
    var elemCursor = body
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
    # Array is a wrapper struct containing the actual array
    result = "%" & name & " = type { [" & sizeStr & " x " & elemType & "] }\n"
  of EnumT:
    # Enums are just their underlying integer type
    inc body
    var baseCursor = body
    let baseType = genTypeLLVM(c, body)
    # Skip enum fields
    while body.kind != ParRi:
      skip body
    inc body
    result = "%" & name & " = type { " & baseType & " }\n"
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
      skip n # type
      var fields: seq[string] = @[]
      while n.kind != ParRi:
        if n.substructureKind == KvU:
          inc n
          skip n # field name
          fields.add genConstantLLVM(c, n, "")
          if n.kind != ParRi: skip n # optional inheritance depth
          skipParRi n
        elif n.exprKind == OconstrC:
          fields.add genConstantLLVM(c, n, "")
        else:
          fields.add genConstantLLVM(c, n, "")
      skipParRi n
      result = "{ " & fields.join(", ") & " }"
    of AconstrC:
      inc n
      skip n # type
      var elems: seq[string] = @[]
      while n.kind != ParRi:
        elems.add genConstantLLVM(c, n, "")
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
