#
#
#           NIFC Compiler LLVM Backend
#        (c) Copyright 2024
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from llvm.nim

proc genStmt(c: var GeneratedCode; n: var Cursor)

proc genEmitStmt(c: var GeneratedCode; n: var Cursor) =
  # Emit statements in LLVM would need to be handled as inline assembly
  # This is a complex operation and not fully implemented here
  error c.m, "emit statements not yet supported in LLVM backend", n
  skip n

proc genBlock(c: var GeneratedCode; n: var Cursor) =
  inc n
  let function = LLVMGetBasicBlockParent(c.currentBlock)

  # Create blocks for break/continue
  let exitBlock = LLVMAppendBasicBlockInContext(c.context, function, "block.exit")

  # Process block statements
  c.m.openScope()
  genStmt(c, n)
  c.m.closeScope()

  # Jump to exit block if not terminated
  if LLVMIsATerminatorInst(LLVMGetLastInstruction(c.currentBlock)) == nil:
    discard LLVMBuildBr(c.builder, exitBlock)

  # Position at exit block
  LLVMPositionBuilderAtEnd(c.builder, exitBlock)
  c.currentBlock = exitBlock

  skipParRi n # Skip the closing parenthesis of the block

proc genReturn(c: var GeneratedCode; n: var Cursor) =
  inc n
  if n.kind == DotToken:
    # Void return
    discard LLVMBuildRetVoid(c.builder)
  else:
    # Return with value
    let retVal = genx(c, n)
    discard LLVMBuildRet(c.builder, retVal)
  skipParRi n

proc genIf(c: var GeneratedCode; n: var Cursor) =
  inc n
  let condition = genx(c, n)

  # Convert condition to boolean if needed
  let boolType = LLVMInt1TypeInContext(c.context)
  let condBool = LLVMBuildICmp(c.builder, LLVMIntNE, condition,
                              LLVMConstInt(LLVMTypeOf(condition), 0, 0.LLVMBool), "ifcond")

  # Create basic blocks for then, else, and merge
  let function = LLVMGetBasicBlockParent(c.currentBlock)
  let thenBlock = LLVMAppendBasicBlockInContext(c.context, function, "then")
  let elseBlock = LLVMAppendBasicBlockInContext(c.context, function, "else")
  let mergeBlock = LLVMAppendBasicBlockInContext(c.context, function, "endif")

  # Branch based on condition
  discard LLVMBuildCondBr(c.builder, condBool, thenBlock, elseBlock)

  # Generate code for then branch
  LLVMPositionBuilderAtEnd(c.builder, thenBlock)
  c.m.openScope()
  genStmt(c, n)
  c.m.closeScope()

  # Jump to merge if not terminated
  if LLVMIsATerminatorInst(LLVMGetLastInstruction(thenBlock)) == nil:
    discard LLVMBuildBr(c.builder, mergeBlock)

  # Generate code for else branch
  LLVMPositionBuilderAtEnd(c.builder, elseBlock)
  c.m.openScope()
  if n.kind != ParRi:
    # There is an else branch
    genStmt(c, n)
  c.m.closeScope()

  # Jump to merge if not terminated
  if LLVMIsATerminatorInst(LLVMGetLastInstruction(elseBlock)) == nil:
    discard LLVMBuildBr(c.builder, mergeBlock)

  # Position at merge point
  LLVMPositionBuilderAtEnd(c.builder, mergeBlock)
  c.currentBlock = mergeBlock

  skipParRi n # Skip the closing parenthesis of the if statement

proc genWhileDoStmt(c: var GeneratedCode; n: var Cursor) =
  inc n
  let function = LLVMGetBasicBlockParent(c.currentBlock)

  # Create blocks for condition, loop body, and exit
  let condBlock = LLVMAppendBasicBlockInContext(c.context, function, "while.cond")
  let bodyBlock = LLVMAppendBasicBlockInContext(c.context, function, "while.body")
  let exitBlock = LLVMAppendBasicBlockInContext(c.context, function, "while.exit")

  # Jump to condition block
  discard LLVMBuildBr(c.builder, condBlock)

  # Generate condition code
  LLVMPositionBuilderAtEnd(c.builder, condBlock)
  let condition = genx(c, n)

  # Convert condition to boolean if needed
  let boolType = LLVMInt1TypeInContext(c.context)
  let condBool = LLVMBuildICmp(c.builder, LLVMIntNE, condition,
                              LLVMConstInt(LLVMTypeOf(condition), 0, 0.LLVMBool), "whilecond")

  # Branch based on condition
  discard LLVMBuildCondBr(c.builder, condBool, bodyBlock, exitBlock)

  # Generate loop body
  LLVMPositionBuilderAtEnd(c.builder, bodyBlock)
  c.m.openScope()
  genStmt(c, n)
  c.m.closeScope()

  # Jump back to condition
  discard LLVMBuildBr(c.builder, condBlock)

  # Position at exit block
  LLVMPositionBuilderAtEnd(c.builder, exitBlock)
  c.currentBlock = exitBlock

  skipParRi n # Skip the closing parenthesis of the while statement

proc genAssignment(c: var GeneratedCode; n: var Cursor) =
  inc n
  let lvalue = genLValue(c, n)
  let rvalue = genx(c, n)

  # Generate store instruction
  discard LLVMBuildStore(c.builder, rvalue, lvalue)

  skipParRi n # Skip the closing parenthesis of the assignment

proc genVarDecl(c: var GeneratedCode; n: var Cursor) =
  inc n

  # Get variable name
  if n.kind != SymbolDef:
    error c.m, "expected variable name but got: ", n

  let varName = mangle(pool.syms[n.symId])
  inc n

  # Get variable type
  let varType = genLLVMType(c, n)

  # Create alloca instruction for variable
  let varAlloca = LLVMBuildAlloca(c.builder, varType, varName)

  # Store variable in values table
  c.values[varName] = varAlloca

  # Check for initializer
  if n.kind != DotToken:
    let initValue = genx(c, n)
    discard LLVMBuildStore(c.builder, initValue, varAlloca)

  skipParRi n # Skip the closing parenthesis of the var declaration

proc genDiscard(c: var GeneratedCode; n: var Cursor) =
  inc n
  if n.kind != ParRi:
    # Evaluate expression but discard result
    discard genx(c, n)
  inc n

proc genGlobal(c: var GeneratedCode; n: var Cursor) =
  inc n

  # Get variable name
  if n.kind != SymbolDef:
    error c.m, "expected variable name but got: ", n

  let varName = mangle(pool.syms[n.symId])
  inc n

  # Get variable type
  let varType = genLLVMType(c, n)

  # Create global variable
  let globalVar = LLVMAddGlobal(c.llvmModule, varType, varName)

  # Set initializer (default to zero if none provided)
  if n.kind != ParRi:
    let initValue = genx(c, n)
    LLVMSetInitializer(globalVar, initValue)
  else:
    LLVMSetInitializer(globalVar, LLVMConstNull(varType))

  # Store global in globals table
  c.globals[varName] = globalVar

  skipParRi n # Skip the closing parenthesis of the global declaration

proc genProcDecl(c: var GeneratedCode; n: var Cursor) =
  # Save the current state
  let savedFunction = c.currentFunction
  let savedBlock = c.currentBlock

  # Get procedure name
  inc n
  if n.kind != SymbolDef:
    error c.m, "expected procedure name but got: ", n

  let procName = mangle(pool.syms[n.symId])
  inc n

  # Process parameters
  var paramTypes: seq[LLVMTypeRef] = @[]
  var paramNames: seq[string] = @[]
  var isVarArgs = false

  if n.kind == ParLe:
    inc n
    while n.kind != ParRi:
      # Get parameter name
      if n.kind != SymbolDef:
        error c.m, "expected parameter name but got: ", n

      let paramName = mangle(pool.syms[n.symId])
      paramNames.add paramName
      inc n

      # Get parameter type
      let paramType = genLLVMType(c, n)
      paramTypes.add paramType

      # Check for varargs pragma
      if n.kind == ParLe:
        var pragmas = n
        inc pragmas
        while pragmas.kind != ParRi:
          if pragmas.pragmaKind == VarargsP:
            isVarArgs = true
          skip pragmas
        skip n

    inc n # Skip past the parameter list

  # Get return type
  let returnType = if n.kind == DotToken:
    inc n
    LLVMVoidTypeInContext(c.context)
  else:
    genLLVMType(c, n)

  # Create function type
  var paramTypeArray: array[32, LLVMTypeRef] # Assuming max 32 parameters
  for i in 0..<min(paramTypes.len, 32):
    paramTypeArray[i] = paramTypes[i]

  let fnType = LLVMFunctionType(
    returnType,
    if paramTypes.len > 0: addr paramTypeArray[0] else: nil,
    paramTypes.len.cuint,
    (if isVarArgs: 1 else: 0).LLVMBool
  )

  # Create function
  c.currentFunction = LLVMAddFunction(c.llvmModule, procName, fnType)

  # Create entry block
  let entryBlock = LLVMAppendBasicBlockInContext(c.context, c.currentFunction, "entry")
  LLVMPositionBuilderAtEnd(c.builder, entryBlock)
  c.currentBlock = entryBlock

  # Set parameters in values table
  for i in 0..<paramNames.len:
    let param = LLVMGetParam(c.currentFunction, i.cuint)
    let paramAlloca = LLVMBuildAlloca(c.builder, paramTypes[i], paramNames[i])
    discard LLVMBuildStore(c.builder, param, paramAlloca)
    c.values[paramNames[i]] = paramAlloca

  # Process function body
  c.m.openScope()

  # Skip pragmas if present
  skip n

  # Generate function body if not just a declaration
  if n.kind != ParRi:
    genStmt(c, n)

  c.m.closeScope()

  # Add implicit return void if needed
  if LLVMIsATerminatorInst(LLVMGetLastInstruction(c.currentBlock)) == nil and
     LLVMGetTypeKind(returnType) == 0: # Void type
    discard LLVMBuildRetVoid(c.builder)

  # Restore the previous state
  c.currentFunction = savedFunction
  c.currentBlock = savedBlock

  skipParRi n # Skip the closing parenthesis of the proc declaration

proc genTypeDecl(c: var GeneratedCode; n: var Cursor) =
  # Type declarations are handled in the type generation phase
  skip n

proc genStmt(c: var GeneratedCode; n: var Cursor) =
  case n.stmtKind
  #of BlockS:
  #  genBlock(c, n)
  of IfS:
    genIf(c, n)
  of WhileS:
    genWhileDoStmt(c, n)
  of RetS:
    genReturn(c, n)
  of AsgnS:
    genAssignment(c, n)
  of VarS:
    genVarDecl(c, n)
  of DiscardS:
    genDiscard(c, n)
  of EmitS:
    genEmitStmt(c, n)
  of GvarS:
    genGlobal(c, n)
  of ProcS:
    genProcDecl(c, n)
  of TypeS:
    genTypeDecl(c, n)
  of CallS:
    discard genCall(c, n)
  of StmtsS:
    inc n
    while n.kind != ParRi:
      genStmt(c, n)
    inc n
  else:
    error c.m, "statement not yet implemented for LLVM backend: " & $n.stmtKind, n

proc genToplevel*(c: var GeneratedCode; n: var Cursor) =
  case n.stmtKind
  of VarS, GvarS, TypeS, ProcS, EmitS:
    genStmt(c, n)
  of IfS, AsgnS, DiscardS, WhileS, RetS, CallS:
    if c.inToplevel:
      # We're at the toplevel and encountered a statement that should
      # be in a function. Create a main function implicitly.
      if gfProducesMainProc in c.flags:
        let returnType = LLVMInt32TypeInContext(c.context)
        let fnType = LLVMFunctionType(returnType, nil, 0, 0.LLVMBool)
        c.currentFunction = LLVMAddFunction(c.llvmModule, "main", fnType)
        let entryBlock = LLVMAppendBasicBlockInContext(c.context, c.currentFunction, "entry")
        LLVMPositionBuilderAtEnd(c.builder, entryBlock)
        c.currentBlock = entryBlock
        c.inToplevel = false

        # Process function body
        c.m.openScope()
        while n.kind != ParRi:
          genStmt(c, n)
        c.m.closeScope()

        # Add return 0 at the end of main
        discard LLVMBuildRet(c.builder, LLVMConstInt(returnType, 0, 0.LLVMBool))

        c.inToplevel = true
      else:
        error c.m, "statement not allowed at top level: ", n
    else:
      genStmt(c, n)
  of StmtsS:
    inc n
    while n.kind != ParRi:
      genToplevel(c, n)
    inc n
  else:
    error c.m, "unexpected statement at top level: ", n
