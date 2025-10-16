#
#
#           NIFC Compiler LLVM Backend
#        (c) Copyright 2024
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvm.nim

proc genx(c: var GeneratedCode; n: var Cursor): LLVMValueRef

proc genCall(c: var GeneratedCode; n: var Cursor): LLVMValueRef =
  inc n
  let funcValue = genx(c, n)
  var args: seq[LLVMValueRef] = @[]

  # Get function type and return type
  let funcType = LLVMTypeOf(funcValue)
  let returnType = LLVMGetReturnType(funcType)

  # Process arguments
  while n.kind != ParRi:
    args.add genx(c, n)

  # Create array for arguments
  var argsArray: array[32, LLVMValueRef] # Assuming max 32 args for simplicity
  for i in 0..<min(args.len, 32):
    argsArray[i] = args[i]

  # Build call instruction
  result = LLVMBuildCall2(
    c.builder,
    funcType,
    funcValue,
    if args.len > 0: addr argsArray[0] else: nil,
    args.len.cuint,
    "call"
  )

  skipParRi n

proc genCallCanRaise(c: var GeneratedCode; n: var Cursor): LLVMValueRef =
  inc n
  skip n # Skip error action
  let funcValue = genx(c, n)
  var args: seq[LLVMValueRef] = @[]

  # Get function type and return type
  let funcType = LLVMTypeOf(funcValue)
  let returnType = LLVMGetReturnType(funcType)

  # Process arguments
  while n.kind != ParRi:
    args.add genx(c, n)

  # Create array for arguments
  var argsArray: array[32, LLVMValueRef] # Assuming max 32 args for simplicity
  for i in 0..<min(args.len, 32):
    argsArray[i] = args[i]

  # Build call instruction
  result = LLVMBuildCall2(
    c.builder,
    funcType,
    funcValue,
    if args.len > 0: addr argsArray[0] else: nil,
    args.len.cuint,
    "call"
  )

  skipParRi n

proc genIntLit(c: var GeneratedCode; litId: IntId): LLVMValueRef =
  let i = pool.integers[litId]
  let intType = if i > low(int32) and i <= high(int32):
    LLVMInt32TypeInContext(c.context)
  else:
    LLVMInt64TypeInContext(c.context)
  result = LLVMConstInt(intType, i.culonglong, 1.LLVMBool) # 1 = signed

proc genUIntLit(c: var GeneratedCode; litId: UIntId): LLVMValueRef =
  let i = pool.uintegers[litId]
  let intType = if i <= high(uint32):
    LLVMInt32TypeInContext(c.context)
  else:
    LLVMInt64TypeInContext(c.context)
  result = LLVMConstInt(intType, i.culonglong, 0.LLVMBool) # 0 = unsigned

proc genFloatLit(c: var GeneratedCode; litId: FloatId): LLVMValueRef =
  let f = pool.floats[litId]
  let floatType = LLVMDoubleTypeInContext(c.context)
  result = LLVMConstReal(floatType, f)

proc genCharLit(c: var GeneratedCode; ch: char): LLVMValueRef =
  let charType = LLVMInt8TypeInContext(c.context)
  result = LLVMConstInt(charType, ch.culonglong, 0.LLVMBool)

proc genStringLit(c: var GeneratedCode; litId: StrId): LLVMValueRef =
  let str = pool.strings[litId]
  let charType = LLVMInt8TypeInContext(c.context)
  let strLength = str.len + 1 # +1 for null terminator

  # Create global string constant
  var chars: seq[LLVMValueRef] = @[]
  for ch in str:
    chars.add LLVMConstInt(charType, ch.culonglong, 0.LLVMBool)
  # Add null terminator
  chars.add LLVMConstInt(charType, 0, 0.LLVMBool)

  # Create array constant for string
  let arrayType = LLVMArrayType(charType, strLength.cuint)
  var charArray: array[1024, LLVMValueRef] # Assuming max string length of 1024
  for i in 0..<min(chars.len, 1024):
    charArray[i] = chars[i]

  let stringConstant = LLVMConstArray(
    charType,
    addr charArray[0],
    strLength.cuint
  )

  # Create global variable for string
  let globalString = LLVMAddGlobal(
    c.llvmModule,
    arrayType,
    ("str" & $litId.int).cstring
  )

  # Set string value and make it constant/private
  LLVMSetInitializer(globalString, stringConstant)
  LLVMSetGlobalConstant(globalString, 1.LLVMBool)
  LLVMSetLinkage(globalString, 1) # Private linkage

  # Create GEP to get pointer to first character
  let zero = LLVMConstInt(LLVMInt32TypeInContext(c.context), 0, 0.LLVMBool)
  var indices: array[2, LLVMValueRef] = [zero, zero]

  result = LLVMConstGEP2(
    arrayType,
    globalString,
    addr indices[0],
    2.cuint
  )

proc genDeref(c: var GeneratedCode; n: var Cursor): LLVMValueRef =
  inc n
  let ptrValue = genx(c, n)
  let ptrType = LLVMTypeOf(ptrValue)
  let elemType = LLVMGetElementType(ptrType)

  result = LLVMBuildLoad2(c.builder, elemType, ptrValue, "deref")

  if n.kind != ParRi and n.typeQual == CppRefQ:
    # Handle C++ references - no special handling needed in LLVM
    skip n

  skipParRi n

proc genLValue(c: var GeneratedCode; n: var Cursor): LLVMValueRef

proc genAddr(c: var GeneratedCode; n: var Cursor): LLVMValueRef =
  inc n
  let value = if n.exprKind in {DerefC, PatC, AtC, DotC}:
    # For lvalues, we can get the address directly
    genLValue(c, n)
  else:
    # For expressions, we need to allocate a temporary and store the value
    let exprValue = genx(c, n)
    let exprType = LLVMTypeOf(exprValue)
    let tempVar = LLVMBuildAlloca(c.builder, exprType, "temp")
    discard LLVMBuildStore(c.builder, exprValue, tempVar)
    tempVar

  if n.kind != ParRi and n.typeQual == CppRefQ:
    # Handle C++ references - no special handling needed in LLVM
    skip n

  skipParRi n
  result = value

proc genLValue(c: var GeneratedCode; n: var Cursor): LLVMValueRef =
  case n.exprKind
  of NoExpr:
    if n.kind == Symbol:
      let name = mangle(pool.syms[n.symId])
      # Check if this is a local variable or a global
      if c.values.hasKey(name):
        result = c.values[name]
      elif c.globals.hasKey(name):
        result = c.globals[name]
      else:
        error c.m, "undeclared variable: " & name, n
      inc n
    else:
      error c.m, "expected expression but got: ", n
  of DerefC:
    result = genx(c, n)
  of AtC:
    inc n
    let arrayValue = genx(c, n)
    let index = genx(c, n)

    # Generate a GEP instruction to get element pointer
    let arrayType = LLVMTypeOf(arrayValue)
    var indices: array[2, LLVMValueRef] = [
      LLVMConstInt(LLVMInt32TypeInContext(c.context), 0, 0.LLVMBool),
      index
    ]

    result = LLVMBuildGEP2(
      c.builder,
      LLVMGetElementType(arrayType),
      arrayValue,
      addr indices[0],
      2.cuint,
      "arrayelem"
    )

    skipParRi n
  of PatC:
    inc n
    let pointerValue = genx(c, n)
    let index = genx(c, n)

    # Generate a GEP instruction to get element pointer
    let ptrType = LLVMTypeOf(pointerValue)
    let elemType = LLVMGetElementType(ptrType)

    var indices: array[1, LLVMValueRef] = [index]

    result = LLVMBuildGEP2(
      c.builder,
      elemType,
      pointerValue,
      addr indices[0],
      1.cuint,
      "ptrelem"
    )

    skipParRi n
  of DotC:
    inc n
    let structValue = genx(c, n)
    let fieldName = n
    skip n

    # Get inheritance depth
    if n.kind != IntLit:
      error c.m, "expected integer literal (inheritance depth) but got: ", n
    let depth = pool.integers[n.intId]
    inc n

    # Get struct type and compute field index
    let structType = LLVMTypeOf(structValue)
    let realStructType = LLVMGetElementType(structType)

    # Get field index - this would require more information from the type system
    var fieldIndex = 0
    # TODO: Calculate proper field index based on field name and struct type

    # Create GEP instruction to get field address
    var indices: array[2, LLVMValueRef] = [
      LLVMConstInt(LLVMInt32TypeInContext(c.context), 0, 0.LLVMBool),
      LLVMConstInt(LLVMInt32TypeInContext(c.context), fieldIndex.culonglong, 0.LLVMBool)
    ]

    result = LLVMBuildGEP2(
      c.builder,
      realStructType,
      structValue,
      addr indices[0],
      2.cuint,
      "structfield"
    )

    skipParRi n
  of ErrvC:
    # Global error variable
    if c.globals.hasKey("NIFC_ERR_"):
      result = c.globals["NIFC_ERR_"]
    else:
      # Create global error variable if it doesn't exist
      let boolType = LLVMInt1TypeInContext(c.context)
      let errVar = LLVMAddGlobal(c.llvmModule, boolType, "NIFC_ERR_")
      LLVMSetInitializer(errVar, LLVMConstInt(boolType, 0, 0.LLVMBool))
      c.globals["NIFC_ERR_"] = errVar
      result = errVar
    skip n
  else:
    error c.m, "expected lvalue but got: ", n

proc floatOpNotSupported(c: var GeneratedCode; n: var Cursor; opName: string): LLVMValueRef =
  error(c.m, opName & " operation not supported for floating point", n)
  nil

template genBinOp(c: var GeneratedCode; n: var Cursor;
              intFnName, floatFnName: untyped; opName: string = ""; allowFloat: bool = true): LLVMValueRef =
  inc n
  let leftValue = genx(c, n)
  let rightValue = genx(c, n)

  # Check if we're dealing with integer or floating point
  let leftType = LLVMTypeOf(leftValue)
  let isFloat = LLVMGetTypeKind(leftType) in {15, 16} # Float or Double

  let res = if isFloat:
    if allowFloat:
      floatFnName(c.builder, leftValue, rightValue, "fbinop")
    else:
      error(c.m, opName & " operation not supported for floating point", n)
      nil
  else:
    intFnName(c.builder, leftValue, rightValue, "ibinop")

  skipParRi n
  res

proc genIntCompare(c: var GeneratedCode; n: var Cursor; predicate: cint): LLVMValueRef =
  inc n
  let leftValue = genx(c, n)
  let rightValue = genx(c, n)

  # Generate comparison instruction
  result = LLVMBuildICmp(c.builder, predicate, leftValue, rightValue, "icmp")

  skipParRi n

proc genFloatCompare(c: var GeneratedCode; n: var Cursor; predicate: cint): LLVMValueRef =
  inc n
  let leftValue = genx(c, n)
  let rightValue = genx(c, n)

  # Generate comparison instruction
  result = LLVMBuildFCmp(c.builder, predicate, leftValue, rightValue, "fcmp")

  skipParRi n

proc genCast(c: var GeneratedCode; n: var Cursor): LLVMValueRef =
  inc n
  let targetType = genLLVMType(c, n)
  let value = genx(c, n)
  let sourceType = LLVMTypeOf(value)

  # Determine the kind of cast needed based on the types
  let sourceKind = LLVMGetTypeKind(sourceType)
  let targetKind = LLVMGetTypeKind(targetType)

  result = case sourceKind
  of 12, 13, 14: # PointerTypes
    case targetKind
    of 12, 13, 14: # PointerTypes
      LLVMBuildBitCast(c.builder, value, targetType, "ptrtyptrcast")
    of 8, 9, 10, 11: # IntegerTypes
      LLVMBuildPtrToInt(c.builder, value, targetType, "ptrtointcast")
    else:
      error c.m, "unsupported cast from pointer", n
      nil
  of 8, 9, 10, 11: # IntegerTypes
    case targetKind
    of 12, 13, 14: # PointerTypes
      LLVMBuildIntToPtr(c.builder, value, targetType, "inttoptrcast")
    of 8, 9, 10, 11: # IntegerTypes
      if LLVMGetIntTypeWidth(sourceType) < LLVMGetIntTypeWidth(targetType):
        # Extension
        if LLVMIsConstant(value) != 0:
          LLVMConstSExtOrBitCast(value, targetType)
        else:
          LLVMBuildSExt(c.builder, value, targetType, "sextcast")
      elif LLVMGetIntTypeWidth(sourceType) > LLVMGetIntTypeWidth(targetType):
        # Truncation
        if LLVMIsConstant(value) != 0:
          LLVMConstTrunc(value, targetType)
        else:
          LLVMBuildTrunc(c.builder, value, targetType, "trunccast")
      else:
        # Same size, but maybe different signedness
        if LLVMIsConstant(value) != 0:
          LLVMConstBitCast(value, targetType)
        else:
          LLVMBuildBitCast(c.builder, value, targetType, "bitcast")
    of 15, 16: # Float, Double
      if LLVMIsConstant(value) != 0:
        LLVMConstSIToFP(value, targetType)
      else:
        LLVMBuildSIToFP(c.builder, value, targetType, "inttofpcast")
    else:
      error c.m, "unsupported cast from integer", n
      nil
  of 15, 16: # Float, Double
    case targetKind
    of 8, 9, 10, 11: # IntegerTypes
      if LLVMIsConstant(value) != 0:
        LLVMConstFPToSI(value, targetType)
      else:
        LLVMBuildFPToSI(c.builder, value, targetType, "fptointcast")
    of 15, 16: # Float, Double
      if LLVMGetTypeKind(sourceType) == 15 and LLVMGetTypeKind(targetType) == 16: # Float to Double
        if LLVMIsConstant(value) != 0:
          LLVMConstFPExt(value, targetType)
        else:
          LLVMBuildFPExt(c.builder, value, targetType, "fpextcast")
      elif LLVMGetTypeKind(sourceType) == 16 and LLVMGetTypeKind(targetType) == 15: # Double to Float
        if LLVMIsConstant(value) != 0:
          LLVMConstFPTrunc(value, targetType)
        else:
          LLVMBuildFPTrunc(c.builder, value, targetType, "fptrunccast")
      else:
        # Same type, no cast needed
        value
    else:
      error c.m, "unsupported cast from float", n
      nil
  else:
    error c.m, "unsupported cast", n
    nil

  skipParRi n

proc genx(c: var GeneratedCode; n: var Cursor): LLVMValueRef =
  case n.exprKind
  of NoExpr:
    case n.kind
    of IntLit:
      result = genIntLit(c, n.intId)
      inc n
    of UIntLit:
      result = genUIntLit(c, n.uintId)
      inc n
    of FloatLit:
      result = genFloatLit(c, n.floatId)
      inc n
    of CharLit:
      result = genCharLit(c, n.charLit)
      inc n
    of StringLit:
      result = genStringLit(c, n.litId)
      inc n
    else:
      # Load value from lvalue
      let lvaluePtr = genLValue(c, n)
      let ptrType = LLVMTypeOf(lvaluePtr)
      let valueType = LLVMGetElementType(ptrType)
      result = LLVMBuildLoad2(c.builder, valueType, lvaluePtr, "load")
  of FalseC:
    let boolType = LLVMInt1TypeInContext(c.context)
    result = LLVMConstInt(boolType, 0, 0.LLVMBool)
    skip n
  of TrueC:
    let boolType = LLVMInt1TypeInContext(c.context)
    result = LLVMConstInt(boolType, 1, 0.LLVMBool)
    skip n
  of NilC:
    let ptrType = LLVMPointerType(LLVMInt8TypeInContext(c.context), 0)
    result = LLVMConstNull(ptrType)
    skip n
  of InfC:
    let doubleType = LLVMDoubleTypeInContext(c.context)
    result = LLVMConstReal(doubleType, Inf)
    skip n
  of NegInfC:
    let doubleType = LLVMDoubleTypeInContext(c.context)
    result = LLVMConstReal(doubleType, NegInf)
    skip n
  of NanC:
    let doubleType = LLVMDoubleTypeInContext(c.context)
    result = LLVMConstReal(doubleType, NaN)
    skip n
  of CallC:
    result = genCall(c, n)
  of DerefC:
    result = genDeref(c, n)
  of AddrC:
    result = genAddr(c, n)
  of AddC:
    result = genBinOp(c, n, LLVMBuildAdd, LLVMBuildFAdd)
  of SubC:
    result = genBinOp(c, n, LLVMBuildSub, LLVMBuildFSub)
  of MulC:
    result = genBinOp(c, n, LLVMBuildMul, LLVMBuildFMul)
  of DivC:
    result = genBinOp(c, n, LLVMBuildSDiv, LLVMBuildFDiv)
  of ModC:
    result = genBinOp(c, n, LLVMBuildSRem, LLVMBuildFRem)
  of ShlC:
    result = genBinOp(c, n, LLVMBuildShl, LLVMBuildShl, "shift", false)
  of ShrC:
    result = genBinOp(c, n, LLVMBuildLShr, LLVMBuildLShr, "shift", false)
  of BitandC:
    result = genBinOp(c, n, LLVMBuildAnd, LLVMBuildAnd, "bitwise", false)
  of BitorC:
    result = genBinOp(c, n, LLVMBuildOr, LLVMBuildOr, "bitwise", false)
  of BitxorC:
    result = genBinOp(c, n, LLVMBuildXor, LLVMBuildXor, "bitwise", false)
  of BitnotC:
    inc n
    let value = genx(c, n)
    result = LLVMBuildNot(c.builder, value, "bitnot")
    skipParRi n
  of NotC:
    inc n
    let value = genx(c, n)

    # Convert to i1 boolean
    let boolType = LLVMInt1TypeInContext(c.context)
    let boolValue = LLVMBuildICmp(c.builder, LLVMIntNE, value,
                                  LLVMConstInt(LLVMTypeOf(value), 0, 0.LLVMBool), "tobool")

    # Invert the boolean
    result = LLVMBuildNot(c.builder, boolValue, "not")
    skipParRi n
  of NegC:
    inc n
    let value = genx(c, n)

    # Check if this is integer or float
    let valueType = LLVMTypeOf(value)
    let isFloat = LLVMGetTypeKind(valueType) in {15, 16} # Float or Double

    result = if isFloat:
      LLVMBuildFNeg(c.builder, value, "fneg")
    else:
      LLVMBuildNeg(c.builder, value, "neg")

    skipParRi n
  of EqC:
    result = genIntCompare(c, n, LLVMIntEQ)
  of NeqC:
    result = genIntCompare(c, n, LLVMIntNE)
  of LeC:
    result = genIntCompare(c, n, LLVMIntSLE)
  of LtC:
    result = genIntCompare(c, n, LLVMIntSLT)
  of AndC:
    inc n
    let leftValue = genx(c, n)

    # Create basic blocks for short-circuit evaluation
    let function = LLVMGetBasicBlockParent(c.currentBlock)
    let rightBB = LLVMAppendBasicBlockInContext(c.context, function, "and.rhs")
    let mergeBB = LLVMAppendBasicBlockInContext(c.context, function, "and.end")

    # Convert left value to boolean and branch
    let boolType = LLVMInt1TypeInContext(c.context)
    let leftBool = LLVMBuildICmp(c.builder, LLVMIntNE, leftValue,
                                LLVMConstInt(LLVMTypeOf(leftValue), 0, 0.LLVMBool), "tobool")

    discard LLVMBuildCondBr(c.builder, leftBool, rightBB, mergeBB)

    # Right hand side evaluation
    LLVMPositionBuilderAtEnd(c.builder, rightBB)
    let rightValue = genx(c, n)
    let rightBool = LLVMBuildICmp(c.builder, LLVMIntNE, rightValue,
                                 LLVMConstInt(LLVMTypeOf(rightValue), 0, 0.LLVMBool), "tobool")

    let rightBlock = LLVMGetInsertBlock(c.builder)
    discard LLVMBuildBr(c.builder, mergeBB)

    # Merge block with PHI node
    LLVMPositionBuilderAtEnd(c.builder, mergeBB)
    let phi = LLVMBuildPhi(c.builder, boolType, "and.result")

    let phiValues: array[2, LLVMValueRef] = [LLVMConstInt(boolType, 0, 0.LLVMBool), rightBool]
    let phiBlocks: array[2, LLVMBasicBlockRef] = [c.currentBlock, rightBlock]
    LLVMAddIncoming(phi, addr phiValues[0], addr phiBlocks[0], 2)

    # Update current block
    c.currentBlock = mergeBB
    result = phi

    skipParRi n
  of OrC:
    inc n
    let leftValue = genx(c, n)

    # Create basic blocks for short-circuit evaluation
    let function = LLVMGetBasicBlockParent(c.currentBlock)
    let rightBB = LLVMAppendBasicBlockInContext(c.context, function, "or.rhs")
    let mergeBB = LLVMAppendBasicBlockInContext(c.context, function, "or.end")

    # Convert left value to boolean and branch
    let boolType = LLVMInt1TypeInContext(c.context)
    let leftBool = LLVMBuildICmp(c.builder, LLVMIntNE, leftValue,
                                LLVMConstInt(LLVMTypeOf(leftValue), 0, 0.LLVMBool), "tobool")

    discard LLVMBuildCondBr(c.builder, leftBool, mergeBB, rightBB)

    # Right hand side evaluation
    LLVMPositionBuilderAtEnd(c.builder, rightBB)
    let rightValue = genx(c, n)
    let rightBool = LLVMBuildICmp(c.builder, LLVMIntNE, rightValue,
                                 LLVMConstInt(LLVMTypeOf(rightValue), 0, 0.LLVMBool), "tobool")

    let rightBlock = LLVMGetInsertBlock(c.builder)
    discard LLVMBuildBr(c.builder, mergeBB)

    # Merge block with PHI node
    LLVMPositionBuilderAtEnd(c.builder, mergeBB)
    let phi = LLVMBuildPhi(c.builder, boolType, "or.result")

    let phiValues: array[2, LLVMValueRef] = [LLVMConstInt(boolType, 1, 0.LLVMBool), rightBool]
    let phiBlocks: array[2, LLVMBasicBlockRef] = [c.currentBlock, rightBlock]
    LLVMAddIncoming(phi, addr phiValues[0], addr phiBlocks[0], 2)

    # Update current block
    c.currentBlock = mergeBB
    result = phi

    skipParRi n
  of CastC, ConvC:
    result = genCast(c, n)
  of SizeofC:
    inc n
    let targetType = genLLVMType(c, n)
    result = LLVMSizeOf(targetType)
    skipParRi n
  of AlignofC:
    inc n
    let targetType = genLLVMType(c, n)
    result = LLVMAlignOf(targetType)
    skipParRi n
  of OffsetofC:
    inc n
    let structType = genLLVMType(c, n)
    let fieldName = mangle(pool.syms[n.symId])
    inc n

    # This is tricky to implement in LLVM
    # We'd need to know the field index, which requires type information

    # Creating a dummy struct and using GEP with zero index to get the offset
    let dummyStruct = LLVMConstNull(structType)
    let zero = LLVMConstInt(LLVMInt32TypeInContext(c.context), 0, 0.LLVMBool)

    # Assuming we know field index (would need to be computed)
    let fieldIndex = 0 # TODO: Calculate proper field index

    var indices: array[2, LLVMValueRef] = [zero, LLVMConstInt(LLVMInt32TypeInContext(c.context), fieldIndex.culonglong, 0.LLVMBool)]

    let gep = LLVMConstGEP2(structType, dummyStruct, addr indices[0], 2)
    result = LLVMPtrToInt(gep, LLVMInt64TypeInContext(c.context))

    skipParRi n
  of SufC, AconstrC, OconstrC, ParC, AtC, DotC, PatC, ErrvC:
    error c.m, "expression not yet implemented for LLVM backend: " & $n.exprKind, n
    skip n
    result = LLVMConstNull(LLVMInt32TypeInContext(c.context))