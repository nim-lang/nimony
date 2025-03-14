#
#
#           NIFC Compiler LLVM Backend
#        (c) Copyright 2024
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## LLVM code generation from NIFC tokens

import std / [assertions, syncio, tables, sets, intsets, formatfloat, strutils, packedsets]
from std / os import changeFileExt, splitFile, extractFilename

include ".." / lib / nifprelude
import mangler, nifc_model, cprelude, noptions, typenav

# LLVM ctx API bindings
type
  LLVMContext = object
  LLVMModule = object
  LLVMBuilder = object
  LLVMType = object
  LLVMValue = object
  LLVMBasicBlock = object

  LLVMContextRef = ptr LLVMContext
  LLVMModuleRef = ptr LLVMModule
  LLVMBuilderRef = ptr LLVMBuilder
  LLVMTypeRef = ptr LLVMType
  LLVMValueRef = ptr LLVMValue
  LLVMBasicBlockRef = ptr LLVMBasicBlock
  LLVMBool = int32

proc LLVMContextCreate(): LLVMContextRef {.importc, cdecl.}
proc LLVMContextDispose(ctx: LLVMContextRef) {.importc, cdecl.}
proc LLVMModuleCreateWithNameInContext(moduleId: cstring, ctx: LLVMContextRef): LLVMModuleRef {.importc, cdecl.}
proc LLVMModuleCreateWithName(moduleId: cstring): LLVMModuleRef {.importc, cdecl.}
proc LLVMDisposeModule(module: LLVMModuleRef) {.importc, cdecl.}
proc LLVMCreateBuilderInContext(ctx: LLVMContextRef): LLVMBuilderRef {.importc, cdecl.}
proc LLVMCreateBuilder(): LLVMBuilderRef {.importc, cdecl.}
proc LLVMDisposeBuilder(builder: LLVMBuilderRef) {.importc, cdecl.}
proc LLVMInt1TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMInt8TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMInt16TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMInt32TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMInt64TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMFloatTypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMDoubleTypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMFunctionType(returnType: LLVMTypeRef, paramTypes: ptr LLVMTypeRef, paramCount: cuint, isVarArg: LLVMBool): LLVMTypeRef {.importc, cdecl.}
proc LLVMStructTypeInContext(ctx: LLVMContextRef, elementTypes: ptr LLVMTypeRef, elementCount: cuint, packed: LLVMBool): LLVMTypeRef {.importc, cdecl.}
proc LLVMStructSetBody(structTy: LLVMTypeRef, elementTypes: ptr LLVMTypeRef, elementCount: cuint, packed: LLVMBool) {.importc, cdecl.}
proc LLVMArrayType(elementType: LLVMTypeRef, elementCount: cuint): LLVMTypeRef {.importc, cdecl.}
proc LLVMPointerType(elementType: LLVMTypeRef, addressSpace: cuint): LLVMTypeRef {.importc, cdecl.}
proc LLVMVoidTypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMAddFunction(module: LLVMModuleRef, name: cstring, functionTy: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMAppendBasicBlockInContext(ctx: LLVMContextRef, fn: LLVMValueRef, name: cstring): LLVMBasicBlockRef {.importc, cdecl.}
proc LLVMBuildRet(builder: LLVMBuilderRef, v: LLVMValueRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildRetVoid(builder: LLVMBuilderRef): LLVMValueRef {.importc, cdecl.}
proc LLVMPositionBuilderAtEnd(builder: LLVMBuilderRef, theBlock: LLVMBasicBlockRef) {.importc, cdecl.}
proc LLVMConstInt(intTy: LLVMTypeRef, n: culonglong, signExtend: LLVMBool): LLVMValueRef {.importc, cdecl.}
proc LLVMConstReal(realTy: LLVMTypeRef, n: cdouble): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildAlloca(builder: LLVMBuilderRef, ty: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildLoad2(builder: LLVMBuilderRef, ty: LLVMTypeRef, pointerVal: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildStore(builder: LLVMBuilderRef, val: LLVMValueRef, thePointer: LLVMValueRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildCall2(builder: LLVMBuilderRef, ty: LLVMTypeRef, fn: LLVMValueRef, args: ptr LLVMValueRef, numArgs: cuint, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildGEP2(builder: LLVMBuilderRef, ty: LLVMTypeRef, thePointer: LLVMValueRef, indices: ptr LLVMValueRef, numIndices: cuint, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildICmp(builder: LLVMBuilderRef, op: cint, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFCmp(builder: LLVMBuilderRef, op: cint, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildBr(builder: LLVMBuilderRef, dest: LLVMBasicBlockRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildCondBr(builder: LLVMBuilderRef, cond: LLVMValueRef, thenPart: LLVMBasicBlockRef, elsePart: LLVMBasicBlockRef): LLVMValueRef {.importc, cdecl.}
proc LLVMPrintModuleToFile(module: LLVMModuleRef, filename: cstring, errorMessage: cstringArray): LLVMBool {.importc, cdecl.}
proc LLVMCreatePassManager(): pointer {.importc, cdecl.}
proc LLVMAddInstructionCombiningPass(pm: pointer) {.importc, cdecl.}
proc LLVMAddReassociatePass(pm: pointer) {.importc, cdecl.}
proc LLVMAddGVNPass(pm: pointer) {.importc, cdecl.}
proc LLVMAddCFGSimplificationPass(pm: pointer) {.importc, cdecl.}
proc LLVMRunPassManager(pm: pointer, module: LLVMModuleRef): LLVMBool {.importc, cdecl.}
proc LLVMDisposePassManager(pm: pointer) {.importc, cdecl.}

# Additional LLVM API functions needed for expressions
proc LLVMTypeOf(val: LLVMValueRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMGetReturnType(functionTy: LLVMTypeRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMGetElementType(ty: LLVMTypeRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMGetTypeKind(ty: LLVMTypeRef): cint {.importc, cdecl.}
proc LLVMGetIntTypeWidth(integerTy: LLVMTypeRef): cuint {.importc, cdecl.}
proc LLVMIsConstant(val: LLVMValueRef): LLVMBool {.importc, cdecl.}
proc LLVMConstSExtOrBitCast(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstTrunc(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstBitCast(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstSIToFP(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstFPToSI(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstFPExt(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstFPTrunc(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSExt(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildTrunc(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildBitCast(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSIToFP(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFPToSI(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFPExt(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFPTrunc(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildPtrToInt(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildIntToPtr(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMConstNull(ty: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstArray(elementTy: LLVMTypeRef, constantVals: ptr LLVMValueRef, length: cuint): LLVMValueRef {.importc, cdecl.}
proc LLVMAddGlobal(module: LLVMModuleRef, ty: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMSetInitializer(globalVar: LLVMValueRef, constantVal: LLVMValueRef) {.importc, cdecl.}
proc LLVMSetGlobalConstant(globalVar: LLVMValueRef, isConstant: LLVMBool) {.importc, cdecl.}
proc LLVMSetLinkage(global: LLVMValueRef, linkage: cuint) {.importc, cdecl.}
proc LLVMConstGEP2(ty: LLVMTypeRef, constantVal: LLVMValueRef, constantIndices: ptr LLVMValueRef, numIndices: cuint): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildNot(builder: LLVMBuilderRef, v: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildNeg(builder: LLVMBuilderRef, v: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFNeg(builder: LLVMBuilderRef, v: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildAdd(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFAdd(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSub(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFSub(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildMul(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFMul(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSDiv(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFDiv(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSRem(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFRem(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildShl(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildLShr(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildAnd(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildOr(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildXor(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMSizeOf(ty: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMAlignOf(ty: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMPtrToInt(val: LLVMValueRef, destTy: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildPhi(builder: LLVMBuilderRef, ty: LLVMTypeRef, name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMAddIncoming(PhiNode: LLVMValueRef, incomingValues: ptr LLVMValueRef, incomingBlocks: ptr LLVMBasicBlockRef, count: cuint) {.importc, cdecl.}
proc LLVMGetInsertBlock(builder: LLVMBuilderRef): LLVMBasicBlockRef {.importc, cdecl.}
proc LLVMGetBasicBlockParent(bb: LLVMBasicBlockRef): LLVMValueRef {.importc, cdecl.}

# Additional LLVM API functions for statements
proc LLVMGetParam(fn: LLVMValueRef, index: cuint): LLVMValueRef {.importc, cdecl.}
proc LLVMIsATerminatorInst(inst: LLVMValueRef): LLVMValueRef {.importc, cdecl.}
proc LLVMGetLastInstruction(bb: LLVMBasicBlockRef): LLVMValueRef {.importc, cdecl.}

const
  LLVMIntEQ: cint = 32
  LLVMIntNE: cint = 33
  LLVMIntUGT: cint = 34
  LLVMIntUGE: cint = 35
  LLVMIntULT: cint = 36
  LLVMIntULE: cint = 37
  LLVMIntSGT: cint = 38
  LLVMIntSGE: cint = 39
  LLVMIntSLT: cint = 40
  LLVMIntSLE: cint = 41
  LLVMRealOEQ: cint = 1
  LLVMRealOGT: cint = 2
  LLVMRealOGE: cint = 3
  LLVMRealOLT: cint = 4
  LLVMRealOLE: cint = 5
  LLVMRealONE: cint = 6

type
  GeneratedCode* = object
    m: Module
    context: LLVMContextRef
    llvmModule: LLVMModuleRef
    builder: LLVMBuilderRef
    currentFunction: LLVMValueRef
    currentBlock: LLVMBasicBlockRef
    types: Table[string, LLVMTypeRef]
    values: Table[string, LLVMValueRef]
    globals: Table[string, LLVMValueRef]
    structs: Table[string, LLVMTypeRef]
    fileIds: PackedSet[FileId]
    generatedTypes: HashSet[SymId]
    flags: set[GenFlag]
    inToplevel: bool
    intmSize: int

proc initGeneratedCode*(m: sink Module, flags: set[GenFlag]): GeneratedCode =
  result = GeneratedCode(
    m: m,
    context: LLVMContextCreate(),
    flags: flags,
    inToplevel: true
  )
  result.llvmModule = LLVMModuleCreateWithNameInContext(result.m.filename.cstring, result.context)
  result.builder = LLVMCreateBuilderInContext(result.context)
  result.types = initTable[string, LLVMTypeRef]()
  result.values = initTable[string, LLVMValueRef]()
  result.globals = initTable[string, LLVMValueRef]()
  result.structs = initTable[string, LLVMTypeRef]()
  result.fileIds = initPackedSet[FileId]()
  result.generatedTypes = initHashSet[SymId]()

proc error(m: Module; msg: string; n: Cursor) {.noreturn.} =
  let info = n.info
  if info.isValid:
    let rawInfo = unpack(pool.man, info)
    if rawInfo.file.isValid:
      write stdout, pool.files[rawInfo.file]
      write stdout, "(" & $rawInfo.line & ", " & $(rawInfo.col+1) & ") "
  write stdout, "[Error] "
  write stdout, msg
  writeLine stdout, toString(n, false)
  when defined(debug):
    echo getStackTrace()
  quit 1

# Type handling
include genllvmtypes

# Expression handling
include genllvmexprs

# Statement handling
include genllvmstmts

proc traverseCode(c: var GeneratedCode; n: var Cursor) =
  if n.stmtKind == StmtsS:
    inc n
    while n.kind != ParRi: genToplevel(c, n)
    # missing `inc n` here is intentional
  else:
    error c.m, "expected `stmts` but got: ", n

proc generateCode(s: var State, inp, outp: string; flags: set[GenFlag]) =
  var m = load(inp)
  m.config = s.config
  var c = initGeneratedCode(m, flags)
  c.m.openScope()

  var co = TypeOrder()
  traverseTypes(c.m, co)

  generateTypes(c, co)

  var n = beginRead(c.m.src)
  traverseCode c, n

  # Optimize the generated code
  let pm = LLVMCreatePassManager()
  LLVMAddInstructionCombiningPass(pm)
  LLVMAddReassociatePass(pm)
  LLVMAddGVNPass(pm)
  LLVMAddCFGSimplificationPass(pm)
  discard LLVMRunPassManager(pm, c.llvmModule)
  LLVMDisposePassManager(pm)

  # Write LLVM IR to file
  var errorMsg: cstringArray = nil
  if LLVMPrintModuleToFile(c.llvmModule, outp, errorMsg) != 0:
    echo "Error writing LLVM IR to file"
    quit 1

  # Clean up LLVM resources
  LLVMDisposeBuilder(c.builder)
  LLVMDisposeModule(c.llvmModule)
  LLVMContextDispose(c.context)

  c.m.closeScope()

proc generateLLVMCode*(s: var State, inp, outp: string; flags: set[GenFlag]) =
  ## Main entry point for LLVM code generation, called from the NIFC compiler
  generateCode(s, inp, outp, flags)