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

# LLVM C API bindings
type
  LLVMContextRef = pointer
  LLVMModuleRef = pointer
  LLVMBuilderRef = pointer
  LLVMTypeRef = pointer
  LLVMValueRef = pointer
  LLVMBasicBlockRef = pointer
  LLVMBool = int32

proc LLVMContextCreate(): LLVMContextRef {.importc, cdecl.}
proc LLVMContextDispose(C: LLVMContextRef) {.importc, cdecl.}
proc LLVMModuleCreateWithNameInContext(ModuleID: cstring, C: LLVMContextRef): LLVMModuleRef {.importc, cdecl.}
proc LLVMModuleCreateWithName(ModuleID: cstring): LLVMModuleRef {.importc, cdecl.}
proc LLVMDisposeModule(M: LLVMModuleRef) {.importc, cdecl.}
proc LLVMCreateBuilderInContext(C: LLVMContextRef): LLVMBuilderRef {.importc, cdecl.}
proc LLVMCreateBuilder(): LLVMBuilderRef {.importc, cdecl.}
proc LLVMDisposeBuilder(Builder: LLVMBuilderRef) {.importc, cdecl.}
proc LLVMInt1TypeInContext(C: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMInt8TypeInContext(C: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMInt16TypeInContext(C: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMInt32TypeInContext(C: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMInt64TypeInContext(C: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMFloatTypeInContext(C: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMDoubleTypeInContext(C: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMFunctionType(ReturnType: LLVMTypeRef, ParamTypes: ptr LLVMTypeRef, ParamCount: cuint, IsVarArg: LLVMBool): LLVMTypeRef {.importc, cdecl.}
proc LLVMStructTypeInContext(C: LLVMContextRef, ElementTypes: ptr LLVMTypeRef, ElementCount: cuint, Packed: LLVMBool): LLVMTypeRef {.importc, cdecl.}
proc LLVMStructSetBody(StructTy: LLVMTypeRef, ElementTypes: ptr LLVMTypeRef, ElementCount: cuint, Packed: LLVMBool) {.importc, cdecl.}
proc LLVMArrayType(ElementType: LLVMTypeRef, ElementCount: cuint): LLVMTypeRef {.importc, cdecl.}
proc LLVMPointerType(ElementType: LLVMTypeRef, AddressSpace: cuint): LLVMTypeRef {.importc, cdecl.}
proc LLVMVoidTypeInContext(C: LLVMContextRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMAddFunction(M: LLVMModuleRef, Name: cstring, FunctionTy: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMAppendBasicBlockInContext(C: LLVMContextRef, Fn: LLVMValueRef, Name: cstring): LLVMBasicBlockRef {.importc, cdecl.}
proc LLVMBuildRet(Builder: LLVMBuilderRef, V: LLVMValueRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildRetVoid(Builder: LLVMBuilderRef): LLVMValueRef {.importc, cdecl.}
proc LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef) {.importc, cdecl.}
proc LLVMConstInt(IntTy: LLVMTypeRef, N: culonglong, SignExtend: LLVMBool): LLVMValueRef {.importc, cdecl.}
proc LLVMConstReal(RealTy: LLVMTypeRef, N: cdouble): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildAlloca(Builder: LLVMBuilderRef, Ty: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildLoad2(Builder: LLVMBuilderRef, Ty: LLVMTypeRef, PointerVal: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildStore(Builder: LLVMBuilderRef, Val: LLVMValueRef, Ptr: LLVMValueRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildCall2(Builder: LLVMBuilderRef, Ty: LLVMTypeRef, Fn: LLVMValueRef, Args: ptr LLVMValueRef, NumArgs: cuint, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildGEP2(Builder: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Indices: ptr LLVMValueRef, NumIndices: cuint, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildICmp(Builder: LLVMBuilderRef, Op: cint, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFCmp(Builder: LLVMBuilderRef, Op: cint, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildBr(Builder: LLVMBuilderRef, Dest: LLVMBasicBlockRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildCondBr(Builder: LLVMBuilderRef, If: LLVMValueRef, Then: LLVMBasicBlockRef, Else: LLVMBasicBlockRef): LLVMValueRef {.importc, cdecl.}
proc LLVMPrintModuleToFile(M: LLVMModuleRef, Filename: cstring, ErrorMessage: cstringArray): LLVMBool {.importc, cdecl.}
proc LLVMCreatePassManager(): pointer {.importc, cdecl.}
proc LLVMAddInstructionCombiningPass(PM: pointer) {.importc, cdecl.}
proc LLVMAddReassociatePass(PM: pointer) {.importc, cdecl.}
proc LLVMAddGVNPass(PM: pointer) {.importc, cdecl.}
proc LLVMAddCFGSimplificationPass(PM: pointer) {.importc, cdecl.}
proc LLVMRunPassManager(PM: pointer, M: LLVMModuleRef): LLVMBool {.importc, cdecl.}
proc LLVMDisposePassManager(PM: pointer) {.importc, cdecl.}

# Additional LLVM API functions needed for expressions
proc LLVMTypeOf(Val: LLVMValueRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMGetReturnType(FunctionTy: LLVMTypeRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMGetElementType(Ty: LLVMTypeRef): LLVMTypeRef {.importc, cdecl.}
proc LLVMGetTypeKind(Ty: LLVMTypeRef): cint {.importc, cdecl.}
proc LLVMGetIntTypeWidth(IntegerTy: LLVMTypeRef): cuint {.importc, cdecl.}
proc LLVMIsConstant(Val: LLVMValueRef): LLVMBool {.importc, cdecl.}
proc LLVMConstSExtOrBitCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstTrunc(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstBitCast(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstSIToFP(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstFPToSI(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstFPExt(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstFPTrunc(ConstantVal: LLVMValueRef, ToType: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSExt(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildTrunc(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildBitCast(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSIToFP(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFPToSI(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFPExt(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFPTrunc(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildPtrToInt(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildIntToPtr(Builder: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMConstNull(Ty: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMConstArray(ElementTy: LLVMTypeRef, ConstantVals: ptr LLVMValueRef, Length: cuint): LLVMValueRef {.importc, cdecl.}
proc LLVMAddGlobal(M: LLVMModuleRef, Ty: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMSetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef) {.importc, cdecl.}
proc LLVMSetGlobalConstant(GlobalVar: LLVMValueRef, IsConstant: LLVMBool) {.importc, cdecl.}
proc LLVMSetLinkage(Global: LLVMValueRef, Linkage: cuint) {.importc, cdecl.}
proc LLVMConstGEP2(Ty: LLVMTypeRef, ConstantVal: LLVMValueRef, ConstantIndices: ptr LLVMValueRef, NumIndices: cuint): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildNot(Builder: LLVMBuilderRef, V: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildNeg(Builder: LLVMBuilderRef, V: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFNeg(Builder: LLVMBuilderRef, V: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildAdd(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFAdd(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSub(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFSub(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildMul(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFMul(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSDiv(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFDiv(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildSRem(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildFRem(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildShl(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildLShr(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildAnd(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildOr(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildXor(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMSizeOf(Ty: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMAlignOf(Ty: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMPtrToInt(Val: LLVMValueRef, DestTy: LLVMTypeRef): LLVMValueRef {.importc, cdecl.}
proc LLVMBuildPhi(Builder: LLVMBuilderRef, Ty: LLVMTypeRef, Name: cstring): LLVMValueRef {.importc, cdecl.}
proc LLVMAddIncoming(PhiNode: LLVMValueRef, IncomingValues: ptr LLVMValueRef, IncomingBlocks: ptr LLVMBasicBlockRef, Count: cuint) {.importc, cdecl.}
proc LLVMGetInsertBlock(Builder: LLVMBuilderRef): LLVMBasicBlockRef {.importc, cdecl.}
proc LLVMGetBasicBlockParent(BB: LLVMBasicBlockRef): LLVMValueRef {.importc, cdecl.}

# Additional LLVM API functions for statements
proc LLVMGetParam(Fn: LLVMValueRef, Index: cuint): LLVMValueRef {.importc, cdecl.}
proc LLVMIsATerminatorInst(Inst: LLVMValueRef): LLVMValueRef {.importc, cdecl.}
proc LLVMGetLastInstruction(BB: LLVMBasicBlockRef): LLVMValueRef {.importc, cdecl.}

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