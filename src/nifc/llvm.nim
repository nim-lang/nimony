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

proc LLVMContextCreate(): LLVMContextRef {.importc: "LLVMContextCreate", cdecl.}
proc LLVMContextDispose(ctx: LLVMContextRef) {.importc: "LLVMContextDispose", cdecl.}
proc LLVMModuleCreateWithNameInContext(moduleId: cstring, ctx: LLVMContextRef): LLVMModuleRef {.importc: "LLVMModuleCreateWithNameInContext", cdecl.}
proc LLVMModuleCreateWithName(moduleId: cstring): LLVMModuleRef {.importc: "LLVMModuleCreateWithName", cdecl.}
proc LLVMDisposeModule(module: LLVMModuleRef) {.importc: "LLVMDisposeModule", cdecl.}
proc LLVMCreateBuilderInContext(ctx: LLVMContextRef): LLVMBuilderRef {.importc: "LLVMCreateBuilderInContext", cdecl.}
proc LLVMCreateBuilder(): LLVMBuilderRef {.importc: "LLVMCreateBuilder", cdecl.}
proc LLVMDisposeBuilder(builder: LLVMBuilderRef) {.importc: "LLVMDisposeBuilder", cdecl.}
proc LLVMInt1TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc: "LLVMInt1TypeInContext", cdecl.}
proc LLVMInt8TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc: "LLVMInt8TypeInContext", cdecl.}
proc LLVMInt16TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc: "LLVMInt16TypeInContext", cdecl.}
proc LLVMInt32TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc: "LLVMInt32TypeInContext", cdecl.}
proc LLVMInt64TypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc: "LLVMInt64TypeInContext", cdecl.}
proc LLVMFloatTypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc: "LLVMFloatTypeInContext", cdecl.}
proc LLVMDoubleTypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc: "LLVMDoubleTypeInContext", cdecl.}
proc LLVMFunctionType(returnType: LLVMTypeRef, paramTypes: ptr LLVMTypeRef, paramCount: cuint, isVarArg: LLVMBool): LLVMTypeRef {.importc: "LLVMFunctionType", cdecl.}
proc LLVMStructTypeInContext(ctx: LLVMContextRef, elementTypes: ptr LLVMTypeRef, elementCount: cuint, packed: LLVMBool): LLVMTypeRef {.importc: "LLVMStructTypeInContext", cdecl.}
proc LLVMStructSetBody(structTy: LLVMTypeRef, elementTypes: ptr LLVMTypeRef, elementCount: cuint, packed: LLVMBool) {.importc: "LLVMStructSetBody", cdecl.}
proc LLVMArrayType(elementType: LLVMTypeRef, elementCount: cuint): LLVMTypeRef {.importc: "LLVMArrayType", cdecl.}
proc LLVMPointerType(elementType: LLVMTypeRef, addressSpace: cuint): LLVMTypeRef {.importc: "LLVMPointerType", cdecl.}
proc LLVMVoidTypeInContext(ctx: LLVMContextRef): LLVMTypeRef {.importc: "LLVMVoidTypeInContext", cdecl.}
proc LLVMAddFunction(module: LLVMModuleRef, name: cstring, functionTy: LLVMTypeRef): LLVMValueRef {.importc: "LLVMAddFunction", cdecl.}
proc LLVMAppendBasicBlockInContext(ctx: LLVMContextRef, fn: LLVMValueRef, name: cstring): LLVMBasicBlockRef {.importc: "LLVMAppendBasicBlockInContext", cdecl.}
proc LLVMBuildRet(builder: LLVMBuilderRef, v: LLVMValueRef): LLVMValueRef {.importc: "LLVMBuildRet", cdecl.}
proc LLVMBuildRetVoid(builder: LLVMBuilderRef): LLVMValueRef {.importc: "LLVMBuildRetVoid", cdecl.}
proc LLVMPositionBuilderAtEnd(builder: LLVMBuilderRef, theBlock: LLVMBasicBlockRef) {.importc: "LLVMPositionBuilderAtEnd", cdecl.}
proc LLVMConstInt(intTy: LLVMTypeRef, n: culonglong, signExtend: LLVMBool): LLVMValueRef {.importc: "LLVMConstInt", cdecl.}
proc LLVMConstReal(realTy: LLVMTypeRef, n: cdouble): LLVMValueRef {.importc: "LLVMConstReal", cdecl.}
proc LLVMBuildAlloca(builder: LLVMBuilderRef, ty: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildAlloca", cdecl.}
proc LLVMBuildLoad2(builder: LLVMBuilderRef, ty: LLVMTypeRef, pointerVal: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildLoad2", cdecl.}
proc LLVMBuildStore(builder: LLVMBuilderRef, val: LLVMValueRef, thePointer: LLVMValueRef): LLVMValueRef {.importc: "LLVMBuildStore", cdecl.}
proc LLVMBuildCall2(builder: LLVMBuilderRef, ty: LLVMTypeRef, fn: LLVMValueRef,
                    args: ptr LLVMValueRef, numArgs: cuint, name: cstring): LLVMValueRef {.importc: "LLVMBuildCall2", cdecl.}
proc LLVMBuildGEP2(builder: LLVMBuilderRef, ty: LLVMTypeRef, thePointer: LLVMValueRef, indices: ptr LLVMValueRef, numIndices: cuint, name: cstring): LLVMValueRef {.importc: "LLVMBuildGEP2", cdecl.}
proc LLVMBuildICmp(builder: LLVMBuilderRef, op: cint, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildICmp", cdecl.}
proc LLVMBuildFCmp(builder: LLVMBuilderRef, op: cint, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFCmp", cdecl.}
proc LLVMBuildBr(builder: LLVMBuilderRef, dest: LLVMBasicBlockRef): LLVMValueRef {.importc: "LLVMBuildBr", cdecl.}
proc LLVMBuildCondBr(builder: LLVMBuilderRef, cond: LLVMValueRef, thenPart: LLVMBasicBlockRef, elsePart: LLVMBasicBlockRef): LLVMValueRef {.importc: "LLVMBuildCondBr", cdecl.}
proc LLVMPrintModuleToFile(module: LLVMModuleRef, filename: cstring, errorMessage: cstringArray): LLVMBool {.importc: "LLVMPrintModuleToFile", cdecl.}
proc LLVMCreatePassManager(): pointer {.importc: "LLVMCreatePassManager", cdecl.}
proc LLVMAddInstructionCombiningPass(pm: pointer) {.importc: "LLVMAddInstructionCombiningPass", cdecl.}
proc LLVMAddReassociatePass(pm: pointer) {.importc: "LLVMAddReassociatePass", cdecl.}
proc LLVMAddGVNPass(pm: pointer) {.importc: "LLVMAddGVNPass", cdecl.}
proc LLVMAddCFGSimplificationPass(pm: pointer) {.importc: "LLVMAddCFGSimplificationPass", cdecl.}
proc LLVMRunPassManager(pm: pointer, module: LLVMModuleRef): LLVMBool {.importc: "LLVMRunPassManager", cdecl.}
proc LLVMDisposePassManager(pm: pointer) {.importc: "LLVMDisposePassManager", cdecl.}

# Additional LLVM API functions needed for expressions
proc LLVMTypeOf(val: LLVMValueRef): LLVMTypeRef {.importc: "LLVMTypeOf", cdecl.}
proc LLVMGetReturnType(functionTy: LLVMTypeRef): LLVMTypeRef {.importc: "LLVMGetReturnType", cdecl.}
proc LLVMGetElementType(ty: LLVMTypeRef): LLVMTypeRef {.importc: "LLVMGetElementType", cdecl.}
proc LLVMGetTypeKind(ty: LLVMTypeRef): cint {.importc: "LLVMGetTypeKind", cdecl.}
proc LLVMGetIntTypeWidth(integerTy: LLVMTypeRef): cuint {.importc: "LLVMGetIntTypeWidth", cdecl.}
proc LLVMIsConstant(val: LLVMValueRef): LLVMBool {.importc: "LLVMIsConstant", cdecl.}
proc LLVMConstSExtOrBitCast(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc: "LLVMConstSExtOrBitCast", cdecl.}
proc LLVMConstTrunc(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc: "LLVMConstTrunc", cdecl.}
proc LLVMConstBitCast(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc: "LLVMConstBitCast", cdecl.}
proc LLVMConstSIToFP(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc: "LLVMConstSIToFP", cdecl.}
proc LLVMConstFPToSI(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc: "LLVMConstFPToSI", cdecl.}
proc LLVMConstFPExt(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc: "LLVMConstFPExt", cdecl.}
proc LLVMConstFPTrunc(constantVal: LLVMValueRef, toType: LLVMTypeRef): LLVMValueRef {.importc: "LLVMConstFPTrunc", cdecl.}
proc LLVMBuildSExt(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildSExt", cdecl.}
proc LLVMBuildTrunc(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildTrunc", cdecl.}
proc LLVMBuildBitCast(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildBitCast", cdecl.}
proc LLVMBuildSIToFP(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildSIToFP", cdecl.}
proc LLVMBuildFPToSI(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFPToSI", cdecl.}
proc LLVMBuildFPExt(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFPExt", cdecl.}
proc LLVMBuildFPTrunc(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFPTrunc", cdecl.}
proc LLVMBuildPtrToInt(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildPtrToInt", cdecl.}
proc LLVMBuildIntToPtr(builder: LLVMBuilderRef, val: LLVMValueRef, destTy: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildIntToPtr", cdecl.}
proc LLVMConstNull(ty: LLVMTypeRef): LLVMValueRef {.importc: "LLVMConstNull", cdecl.}
proc LLVMConstArray(elementTy: LLVMTypeRef, constantVals: ptr LLVMValueRef, length: cuint): LLVMValueRef {.importc: "LLVMConstArray", cdecl.}
proc LLVMAddGlobal(module: LLVMModuleRef, ty: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMAddGlobal", cdecl.}
proc LLVMSetInitializer(globalVar: LLVMValueRef, constantVal: LLVMValueRef) {.importc: "LLVMSetInitializer", cdecl.}
proc LLVMSetGlobalConstant(globalVar: LLVMValueRef, isConstant: LLVMBool) {.importc: "LLVMSetGlobalConstant", cdecl.}
proc LLVMSetLinkage(global: LLVMValueRef, linkage: cuint) {.importc: "LLVMSetLinkage", cdecl.}
proc LLVMConstGEP2(ty: LLVMTypeRef, constantVal: LLVMValueRef, constantIndices: ptr LLVMValueRef, numIndices: cuint): LLVMValueRef {.importc: "LLVMConstGEP2", cdecl.}
proc LLVMBuildNot(builder: LLVMBuilderRef, v: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildNot", cdecl.}
proc LLVMBuildNeg(builder: LLVMBuilderRef, v: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildNeg", cdecl.}
proc LLVMBuildFNeg(builder: LLVMBuilderRef, v: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFNeg", cdecl.}
proc LLVMBuildAdd(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildAdd", cdecl.}
proc LLVMBuildFAdd(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFAdd", cdecl.}
proc LLVMBuildSub(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildSub", cdecl.}
proc LLVMBuildFSub(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFSub", cdecl.}
proc LLVMBuildMul(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildMul", cdecl.}
proc LLVMBuildFMul(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFMul", cdecl.}
proc LLVMBuildSDiv(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildSDiv", cdecl.}
proc LLVMBuildFDiv(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFDiv", cdecl.}
proc LLVMBuildSRem(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildSRem", cdecl.}
proc LLVMBuildFRem(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildFRem", cdecl.}
proc LLVMBuildShl(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildShl", cdecl.}
proc LLVMBuildLShr(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildLShr", cdecl.}
proc LLVMBuildAnd(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildAnd", cdecl.}
proc LLVMBuildOr(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildOr", cdecl.}
proc LLVMBuildXor(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildXor", cdecl.}
proc LLVMSizeOf(ty: LLVMTypeRef): LLVMValueRef {.importc: "LLVMSizeOf", cdecl.}
proc LLVMAlignOf(ty: LLVMTypeRef): LLVMValueRef {.importc: "LLVMAlignOf", cdecl.}
proc LLVMPtrToInt(val: LLVMValueRef, destTy: LLVMTypeRef): LLVMValueRef {.importc: "LLVMPtrToInt", cdecl.}
proc LLVMBuildPhi(builder: LLVMBuilderRef, ty: LLVMTypeRef, name: cstring): LLVMValueRef {.importc: "LLVMBuildPhi", cdecl.}
proc LLVMAddIncoming(phiNode: LLVMValueRef, incomingValues: ptr LLVMValueRef, incomingBlocks: ptr LLVMBasicBlockRef, count: cuint) {.importc: "LLVMAddIncoming", cdecl.}
proc LLVMGetInsertBlock(builder: LLVMBuilderRef): LLVMBasicBlockRef {.importc: "LLVMGetInsertBlock", cdecl.}
proc LLVMGetBasicBlockParent(bb: LLVMBasicBlockRef): LLVMValueRef {.importc: "LLVMGetBasicBlockParent", cdecl.}

# Additional LLVM API functions for statements
proc LLVMGetParam(fn: LLVMValueRef, index: cuint): LLVMValueRef {.importc: "LLVMGetParam", cdecl.}
proc LLVMIsATerminatorInst(inst: LLVMValueRef): LLVMValueRef {.importc: "LLVMIsATerminatorInst", cdecl.}
proc LLVMGetLastInstruction(bb: LLVMBasicBlockRef): LLVMValueRef {.importc: "LLVMGetLastInstruction", cdecl.}

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
