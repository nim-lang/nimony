#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Driver layer: the multi-phase passes over a module's top-level statements
## (phase1/2/3), cycle-group handling, post-processing (generics, contracts,
## derefs, methods) and output writing — plus the `semcheck` entry points used
## by nimsem/nimony.
##
## This sits *above* the sem engine: it imports `sem` and only calls inward, so
## it needs no callbacks. `initSemContext` (here) wires the engine procs into
## the `SemContext` callback vtable.

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
import std / [tables, sets, syncio, assertions, hashes]
from std/os import changeFileExt, getCurrentDir, isAbsolute, absolutePath, normalizedPath
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [symparser, nifindexes, docpaths]
import ".." / gear2 / modnames
import ".." / models / nifindex_tags
import nimony_model, symtabs, builtintypes, decls, programs, sigmatch,
  reporters, nifconfig, xints, semdata, sembasics,
  semos, langmodes, derefs, vtables_frontend,
  contracts_njvl, exprexec, semimport, module_plugins, sem
when not defined(nimony):
  import ".." / validator / phase_validator

type
  ModuleState = object
    owningBuf: TokenBuf
    n0: Cursor
    c: SemContext
    dest: TokenBuf
    outfile: string
    buf1: TokenBuf
    moduleLineInfo: PackedLineInfo

proc loadSymWithPhase*(c: var SemContext; symId: SymId; targetPhase: SemPhase): LoadResult

proc buildIndexExports(c: var SemContext): TokenBuf =
  if c.exports.len == 0:
    return default(TokenBuf)
  result = createTokenBuf(32)
  for m, ex in c.exports:
    let path = toAbsolutePath(c.importedModules.getOrQuit(m).path)
    case ex.kind
    of ImportAll:
      result.addParLe(TagId(ExportIdx), NoLineInfo)
      result.add strToken(pool.strings.getOrIncl(path), NoLineInfo)
      result.addParRi()
    of FromImport:
      if ex.list.len != 0:
        result.addParLe(TagId(FromexportIdx), NoLineInfo)
        result.add strToken(pool.strings.getOrIncl(path), NoLineInfo)
        for s in ex.list:
          result.add identToken(s, NoLineInfo)
        result.addParRi()
    of ImportExcept:
      let kind = if ex.list.len == 0: ExportIdx else: ExportexceptIdx
      result.addParLe(TagId(kind), NoLineInfo)
      result.add strToken(pool.strings.getOrIncl(path), NoLineInfo)
      for s in ex.list:
        result.add identToken(s, NoLineInfo)
      result.addParRi()

proc writeNewDepsFile(c: var SemContext; outfile: string) =
  # Update .s.deps.nif file that doesn't contain modules imported under `when false:`
  # so that Hexer and following phases doesn't read such modules.
  var deps = createTokenBuf(16)
  deps.buildTree StmtsS, NoLineInfo:
    if c.importedModules.len != 0:
      var usedPlugins = false
      deps.buildTree ImportS, NoLineInfo:
        for _, i in c.importedModules:
          if i.fromPlugin.len == 0:
            deps.addStrLit i.path.toAbsolutePath
          else:
            usedPlugins = true
      if usedPlugins:
        for _, i in c.importedModules:
          if i.fromPlugin.len > 0:
            deps.buildTree ImportS, NoLineInfo:
              deps.buildTree PragmaxX, NoLineInfo:
                deps.addStrLit i.path.toAbsolutePath
                deps.buildTree PragmasS, NoLineInfo:
                  deps.buildTree KvU, NoLineInfo:
                    deps.addIdent "plugin"
                    deps.addStrLit i.fromPlugin
    if c.toBuild.len != 0:
      deps.buildTree TagId(BuildIdx), NoLineInfo:
        deps.add c.toBuild
    if c.toBundle.len != 0:
      deps.buildTree TagId(BundleIdx), NoLineInfo:
        deps.add c.toBundle
    if c.passL.len != 0:
      deps.buildTree TagId(PassLP), NoLineInfo:
        for i in c.passL:
          deps.addStrLit i
    if c.passC.len != 0:
      deps.buildTree TagId(PassCP), NoLineInfo:
        for i in c.passC:
          deps.addStrLit i
  let depsFile = changeModuleExt(outfile, ".s.deps.nif")
  onRaiseQuit writeFile(deps, depsFile, OnlyIfChanged)

proc pruneMatchedForwardDecls(c: var SemContext; dest: var TokenBuf) =
  ## Overwrite `(proc :sym ...)` subtrees with DotTokens for every symbol
  ## in `c.matchedForwardDecls`. We can't splice during sem (positions
  ## held by other unmatched forward decls would shift), so we do a
  ## single in-place pass before serialisation. The DotTokens are valid
  ## NIF stand-ins and `derefs.nim` strips them later in the pipeline.
  if c.matchedForwardDecls.len == 0: return
  var i = 0
  while i < dest.len:
    if dest[i].kind == ParLe and i + 1 < dest.len and
        dest[i + 1].kind == SymbolDef and
        dest[i + 1].symId in c.matchedForwardDecls:
      let info = dest[i].info
      var nesting = 0
      while i < dest.len:
        case dest[i].kind
        of ParLe: inc nesting
        of ParRi: dec nesting
        else: discard
        dest[i] = dotToken(info)
        inc i
        if nesting == 0: break
    else:
      inc i

proc writeOutput(c: var SemContext; dest: var TokenBuf; outfile: string) =
  pruneMatchedForwardDecls(c, dest)
  # Insert `(import (kv suffix "path") …)` at the beginning of the (stmts ...)
  # so the hexer sees it before any executable code. The path is paired with
  # the suffix so downstream tools (dagon doc-gen) have the source location
  # without needing a separate manifest. Only consumer of the body today is
  # `nifcgen`'s init-proc generation, which reads the suffix from each `kv`.
  #
  # Path is stored relative to CWD so the `.s.nif` is reproducible across
  # checkouts (CI has a different absolute prefix than the dev's machine).
  # Mirrors nifler's `--portablePaths` line-info convention.
  if c.importedModules.len != 0:
    let curWorkDir = onRaiseQuit os.getCurrentDir()
    var importBuf = createTokenBuf(c.importedModules.len * 5 + 2)
    importBuf.addParLe ImportS, NoLineInfo
    for _, i in c.importedModules:
      if i.fromPlugin.len == 0:
        let abs = i.path.toAbsolutePath
        importBuf.buildTree KvU, NoLineInfo:
          importBuf.addIdent moduleSuffix(abs, c.g.config.paths)
          # Slash-normalise: paths in the .nif must use `/` regardless of OS
          # so the file is byte-identical across Windows / Linux / macOS, and
          # downstream consumers (dagon) compare prefixes uniformly.
          importBuf.addStrLit toUnixPath(abs.toRelativePath(curWorkDir))
    importBuf.addParRi()
    dest.insert importBuf, 1 # after the (stmts tag
  onRaiseQuit writeFile(dest, outfile, OnlyIfChanged)
  let root = dest[0].info
  onRaiseQuit createIndex(outfile, root, true,
    IndexSections(
      converters: move c.converterIndexMap,
      exportBuf: buildIndexExports(c)))
  writeNewDepsFile c, outfile

proc phaseX(c: var SemContext; dest: var TokenBuf; n: Cursor; x: SemPhase) =
  assert n.stmtKind == StmtsS
  c.phase = x
  var n = n
  dest.add n
  n.into:
    while n.hasMore:
      semStmt c, dest, n, false
  dest.addParRi()
  # clear pragmaStack in case {.pop.} was not called
  c.pragmaStack.setLen(0)

proc getModuleLineInfo(buf: var TokenBuf): PackedLineInfo =
  ## Get the line info from the module's StmtsS tag.
  var n = beginRead(buf)
  assert n.stmtKind == StmtsS
  result = n.info
  endRead(buf)

proc ensurePhase*(c: var SemContext; symId: SymId; targetPhase: SemPhase): EnsurePhaseResult =
  ## Check if a symbol has been processed to at least targetPhase.
  ## Used for cycle detection during phase 2/3.
  if not prog.mem.hasKey(symId):
    return PhaseNotFound  # Symbol not in mem (external or not yet registered)

  let currentPhase = prog.mem[symId].phase
  if currentPhase >= targetPhase:
    return PhaseOk  # Already at or past target phase

  # Cycle detection: check for InProgress markers
  if currentPhase in {SemcheckSignaturesInProgress, SemcheckBodiesInProgress}:
    return PhaseCycle

  # Symbol not yet at target phase - this is a forward reference
  # The caller should handle this appropriately
  result = PhaseOk

proc loadSymWithPhase*(c: var SemContext; symId: SymId; targetPhase: SemPhase): LoadResult =
  ## Load a symbol, checking for cycles.
  ## For current module symbols in progress, returns cycle error.
  let phaseRes = ensurePhase(c, symId, targetPhase)
  if phaseRes == PhaseCycle:
    return LoadResult(status: LacksOffset)  # Cycle detected
  result = tryLoadSym(symId)

proc semToplevelStmts(c: var SemContext; dest: var TokenBuf; buf: var TokenBuf) =
  ## Iterate over toplevel statements in buf and semcheck each one.
  var n = beginRead(buf)
  assert n.stmtKind == StmtsS
  n.into:
    while n.hasMore:
      semStmt c, dest, n, false
  endRead(buf)

proc phase1(c: var SemContext; dest: var TokenBuf; n: Cursor): (TokenBuf, PackedLineInfo) =
  ## Phase 1: Register toplevel symbols.
  phaseX(c, dest, n, SemcheckTopLevelSyms)
  let lineInfo = getModuleLineInfo(dest)
  result = (move dest, lineInfo)

proc phase2(c: var SemContext; buf: var TokenBuf; moduleLineInfo: PackedLineInfo): TokenBuf =
  ## Phase 2: Check signatures.
  c.phase = SemcheckSignatures
  result = createTokenBuf()
  result.addParLe(StmtsS, moduleLineInfo)
  semToplevelStmts(c, result, buf)
  result.addParRi()
  c.pragmaStack.setLen(0)

proc phase3(c: var SemContext; buf: var TokenBuf; moduleLineInfo: PackedLineInfo): TokenBuf =
  ## Phase 3: Check bodies.
  c.phase = SemcheckBodies
  result = createTokenBuf()
  result.addParLe(StmtsS, moduleLineInfo)
  semToplevelStmts(c, result, buf)

proc requestHookInstance(c: var SemContext; decl: Cursor) =
  let decl = asTypeDecl(decl)
  var typevars = decl.typevars
  assert classifyType(c, typevars) == InvokeT
  inc typevars
  assert typevars.kind == Symbol

  let symId = typevars.symId

  # For types from the current module, use typeHooks (hooks haven't been embedded
  # in type pragmas yet - that happens in injectDerefs at the end).
  # For types from other modules, use tryLoadAllHooks which reads from type pragmas.
  let moduleSuffix = extractModule(pool.syms[symId])
  let hooks = if moduleSuffix == c.thisModuleSuffix:
      c.typeHooks.getOrDefault(symId)
    else:
      tryLoadAllHooks(symId)
  var needsSomething = false
  for op in low(AttachedOp)..high(AttachedOp):
    let h = hooks.a[op]
    if h != NoSymId:
      needsSomething = true
      break
  if not needsSomething: return

  var inferred = initTable[SymId, Cursor]()
  var typeArgs = createTokenBuf()

  inc typevars # skips symbol

  var typevarsSeq: seq[Cursor] = @[]

  while typevars.hasMore:
    typevarsSeq.add typevars
    takeTree(typeArgs, typevars)

  for op in low(AttachedOp)..high(AttachedOp):
    let hook = hooks.a[op]
    if hook != NoSymId:
      let res = tryLoadSym(hook)
      if res.status == LacksNothing:
        let info = res.decl.info
        let procDecl = asRoutine(res.decl)
        var typevarsStart = procDecl.typevars
        inc typevarsStart # skips typevars tag

        var counter = 0
        while typevarsStart.hasMore:
          let name = asTypevar(typevarsStart).name.symId
          inferred[name] = typevarsSeq[counter]
          skip typevarsStart # skip the typevar tree
          inc counter
        discard requestRoutineInstance(c, hook, typeArgs, inferred, info)
      else:
        quit "BUG: Could not load hook: " & pool.syms[hook]

proc instantiateMethodForType(c: var SemContext; dest: var TokenBuf; methodSym, typeInstSym: SymId): SymId =
  # check if instance actually matches method
  # Returns the instantiated method symbol, or SymId(0) if not applicable
  let res = tryLoadSym(methodSym)
  assert res.status == LacksNothing
  let procDecl = asRoutine(res.decl)
  var firstParam = procDecl.params
  inc firstParam
  firstParam = skipModifier(asLocal(firstParam).typ)
  if firstParam.typeKind in {RefT, PtrT}:
    # instance is the object type, not a ref/ptr type
    inc firstParam
  var typBuf = createTokenBuf(2)
  typBuf.add symToken(typeInstSym, NoLineInfo)
  var paramMatch = createMatch(addr c)
  typematch paramMatch, firstParam, Item(n: emptyNode(c), typ: beginRead(typBuf))
  if classifyMatch(paramMatch) in {EqualMatch, GenericMatch}:
    # type matched, check that the method can be fully instantiated
    var inferred = ensureMove paramMatch.inferred
    var typevars = procDecl.typevars
    inc typevars
    var typeArgsBuf = createTokenBuf(32)
    while typevars.hasMore:
      let name = takeLocal(typevars, SkipFinalParRi).name.symId
      if name notin inferred:
        c.buildErr dest, res.decl.info, "cannot instantiate method " & pool.syms[methodSym] &
          ", cannot infer generic parameter " & pool.syms[name]
        return SymId(0)
      typeArgsBuf.addSubtree inferred.getOrQuit(name)
    let instance = requestRoutineInstance(c, methodSym, typeArgsBuf, inferred, res.decl.info)
    return instance.targetSym
  else:
    # method did not match, fine, consider it unavailable for this instance
    return SymId(0)

proc requestMethods(c: var SemContext; dest: var TokenBuf; s: SymId; decl: Cursor) =
  let decl = asTypeDecl(decl)
  var typevars = decl.typevars
  assert classifyType(c, typevars) == InvokeT
  inc typevars
  assert typevars.kind == Symbol

  let base = typevars.symId

  # Load methods from the base type (from c.classes or type pragmas)
  var baseMethods: seq[MethodIndexEntry]

  # First check if we have a ClassEntry for the base
  if c.classes.hasKey(base):
    baseMethods = c.classes.getOrQuit(base).methods
  else:
    # Try loading from type pragmas
    baseMethods = vtables_frontend.loadVTable(base)
    if baseMethods.len > 0:
      # Create a ClassEntry for the base
      c.classes[base] = ClassEntry(methods: baseMethods)

  # Create a ClassEntry for the generic instance with instantiated methods
  var instanceMethods: seq[MethodIndexEntry] = @[]
  for baseMethod in baseMethods:
    # Instantiate the method for this type instance
    let instantiatedSym = instantiateMethodForType(c, dest, baseMethod.fn, s)
    if instantiatedSym != SymId(0):
      # Use the instantiated symbol, not the base symbol
      instanceMethods.add MethodIndexEntry(fn: instantiatedSym, signature: baseMethod.signature)

  # Store the ClassEntry for this instance
  if instanceMethods.len > 0:
    c.classes[s] = ClassEntry(methods: instanceMethods)

proc addSelfModuleSym(c: var SemContext; path: string) =
  let name = moduleNameFromPath(path)
  let nameId = pool.strings.getOrIncl(name)
  c.selfModuleSym = identToSym(c, nameId, ModuleY)
  let s = Sym(kind: ModuleY, name: c.selfModuleSym, pos: ImportedPos)
  if name != "":
    c.currentScope.addOverloadable(nameId, s)
  var moduleDecl = createTokenBuf(2)
  moduleDecl.addParLe(ModuleY, NoLineInfo)
  moduleDecl.addParRi()
  publish c.selfModuleSym, moduleDecl

proc fromGeneric(dest: var TokenBuf; i: int): SymId =
  var n = cursorAt(dest, i) # at name
  skip n, SkipName # skip name
  skip n, SkipExport # skip exported
  skip n # pattern
  if n.typeKind == InvokeT:
    result = n.firstSon.symId
  else:
    result = NoSymId
  endRead(dest)

proc findOrigin(dest: var TokenBuf; origin: SymId): int =
  var i = 0
  while i < dest.len:
    if dest[i].kind == SymbolDef and dest[i].symId == origin:
      return i-1 # before name
    inc i
  return -1

proc reorderInnerGenericInstances(c: SemContext; dest: var TokenBuf) =
  #[ Consider:

  proc outer =
    var x = 0
    proc inner[T] = echo x
    inner[int]()

  We instantiate `inner` like any other generic but move it below its generic declaration.
  This ensures proper scoping for lambdalifting to pick up later.
  ]#
  var i = 0
  while i < dest.len:
    if dest[i].stmtKind in {ProcS, FuncS, ConverterS, MethodS, MacroS, IteratorS}:
      inc i
      if dest[i].kind == SymbolDef:
        let origin = fromGeneric(dest, i)
        if origin != NoSymId and origin in c.genericInnerProcs:
          # move to right below the position of the origin
          let originPos = findOrigin(dest, origin)
          assert originPos > 0

          let procDecl = cursorAt(dest, i-1)
          var n = procDecl
          skip n
          let procLen = cursorToPosition(dest,n) - (i-1)
          endRead(dest)

          # Extract the procedure declaration
          var procBuf = createTokenBuf(procLen)
          for j in (i-1)..<(i-1+procLen):
            procBuf.add dest[j]
            dest[j] = dotToken(NoLineInfo) # invalidate

          dest.insert procBuf, originPos
    else:
      inc i

proc semcheckCore(c: var SemContext; dest: var TokenBuf; n0: Cursor) =
  c.currentScope = Scope(tab: initTable[StrId, seq[Sym]](), kind: ToplevelScope)

  assert n0.stmtKind == StmtsS
  let path = getFile(n0.info) # gets current module path, maybe there is a better way
  addSelfModuleSym(c, path)

  if {SkipSystem, IsSystem} * c.moduleFlags == {}:
    let systemFile = ImportedFilename(path: stdlibFile("std/system"), name: "system", isSystem: true)
    importSingleFile(c, dest, systemFile, "", ImportFilter(kind: ImportAll), n0.info)

  #echo "PHASE 1"
  var (buf1, moduleLineInfo) = phase1(c, dest, n0)
  #echo "PHASE 2"
  var buf2 = phase2(c, buf1, moduleLineInfo)
  #echo "PHASE 3"
  dest = phase3(c, buf2, moduleLineInfo)

  if c.expanded.len > 0:
    dest.addParLe CommentS, c.expanded[0].info
    dest.add c.expanded
    dest.addParRi()

  instantiateGenerics c, dest
  for val in c.typeInstDecls:
    let s = fetchSym(c, val)
    let res = declToCursor(c, dest, s)
    if res.status == LacksNothing:
      requestHookInstance(c, res.decl)
      requestMethods(c, dest, val, res.decl)
      dest.copyTree res.decl
  dest.add c.pendingSumtypes
  instantiateGenericHooks c, dest
  dest.addParRi()

  if reportErrors(dest) == 0:
    var afterSem = move dest
    if c.genericInnerProcs.len > 0:
      reorderInnerGenericInstances(c, afterSem)
    var finalBuf = beginRead afterSem
    dest = injectDerefs(finalBuf, c.typeHooks, c.classes, c.thisModuleSuffix, c.g.config.bits)
    when true: #defined(enableContracts):
      var moreErrors = analyzeContractsNjvl(dest, c.thisModuleSuffix, c.g.config.verbose)
      if reporters.reportErrors(moreErrors) > 0:
        quit 1
  else:
    quit 1

proc addIfAbsent[T](s: var seq[T]; x: T) =
  for y in s:
    if y == x: return
  s.add x

proc resolveCyclicImports(c: var SemContext) =
  ## Populate importTab and iface for deferred cyclic imports by scanning
  ## prog.mem. Call after phase1 (type symbols) and again after phase2 (proc
  ## symbols). Uses addIfAbsent so repeated calls are safe.
  for (targetSuffix, moduleSym) in c.deferredCyclicImports:
    let module = addr c.importedModules.mgetOrPut(moduleSym, ImportedModule())
    for symId in prog.mem.symIds:
      let symName = pool.syms[symId]
      let modSuffix = extractModule(symName)
      if modSuffix == targetSuffix:
        var baseName = symName
        extractBasename(baseName)
        let nameId = pool.strings.getOrIncl(baseName)
        c.importTab.mgetOrPut(nameId, @[]).addIfAbsent(moduleSym)
        module.iface.mgetOrPut(nameId, @[]).addIfAbsent(symId)

proc initSemContext(suffix: string; config: ProgramContext; moduleFlags: set[ModuleFlag];
                    commandLineArgs: string; canSelfExec: bool): SemContext =
  result = SemContext(
    types: createBuiltinTypes(config.config.bits),
    thisModuleSuffix: suffix,
    moduleFlags: moduleFlags,
    g: config,
    phase: SemcheckTopLevelSyms,
    routine: SemRoutine(kind: NoSym),
    commandLineArgs: commandLineArgs,
    canSelfExec: canSelfExec,
    pending: createTokenBuf(),
    executeExpr: exprexec.executeExpr,
    semStmtCallback: semStmtCallback,
    semGetSize: semGetSize,
    forceInstantiate: forceInstantiateCallback,
    semInstantiateType: instantiateType,
    semExprCB: semExpr,
    semStmtCB: semStmt,
    commonTypeCB: commonType,
    semLocalTypeImplCB: semLocalTypeImpl,
    declareResultCB: declareResult,
    semEmitCB: semEmit)
  for magic in ["typeof", "compiles", "defined", "declared"]:
    result.unoverloadableMagics.incl(pool.strings.getOrIncl(magic))

proc semcheckPostProcess(c: var SemContext; dest: var TokenBuf) =
  ## Post-processing after phase3: generics, contracts, derefs.
  if c.expanded.len > 0:
    dest.addParLe CommentS, c.expanded[0].info
    dest.add c.expanded
    dest.addParRi()

  instantiateGenerics c, dest
  for val in c.typeInstDecls:
    let s = fetchSym(c, val)
    let res = declToCursor(c, dest, s)
    if res.status == LacksNothing:
      requestHookInstance(c, res.decl)
      requestMethods(c, dest, val, res.decl)
      dest.copyTree res.decl
  dest.add c.pendingSumtypes
  instantiateGenericHooks c, dest
  dest.addParRi()

  if reportErrors(dest) == 0:
    var afterSem = move dest
    when true:
      var moreErrors = analyzeContractsNjvl(afterSem, c.thisModuleSuffix, c.g.config.verbose)
      if reporters.reportErrors(moreErrors) > 0:
        quit 1
    if c.genericInnerProcs.len > 0:
      reorderInnerGenericInstances(c, afterSem)
    var finalBuf = beginRead afterSem
    dest = injectDerefs(finalBuf, c.typeHooks, c.classes, c.thisModuleSuffix, c.g.config.bits)
  else:
    quit 1

proc maybeValidatePostSem(dest: var TokenBuf; moduleName: string) =
  ## Validate that `dest` conforms to the post-sem subset of `doc/tags.md`.
  ## Reports violations on stderr and aborts with a non-zero exit status so
  ## that drift from the spec is a hard error. Active by default in host-Nim
  ## builds; nimony's own bootstrap build skips this until the validator
  ## (`phase_validator.nim` / `tags_grammar.nim`) compiles under nimony.
  ## `-d:skipPostSemValidator` opts out (used by Windows CI — see hastur's
  ## `validatePassesFlag`).
  when not defined(nimony) and not defined(skipPostSemValidator):
    let phase = postSemPhase()
    let violations = validate(dest, phase)
    if violations.len > 0:
      stderr.writeLine "[" & moduleName & "] post-sem validator found " &
        $violations.len & " violation(s)" &
        (if violations.len >= 200: " (truncated)" else: "") & ":"
      discard reportViolations(phase.name, violations)
      quit 1

proc semcheckCycleGroup(infiles, outfiles: seq[string]; config: sink NifConfig;
                        moduleFlags: set[ModuleFlag];
                        commandLineArgs: string; canSelfExec: bool) =
  ## Semantic check multiple modules that form a cycle group.
  ## All modules are processed through each phase together:
  ## phase1(all) -> resolve cyclic imports -> phase2(all) ->
  ## resolve cyclic imports -> phase3(all).
  let sharedConfig = ProgramContext(config: config)
  if SkipSystem in moduleFlags:
    programs.publishStringType()

  var modules = newSeqOfCap[ModuleState](infiles.len)
  for i in 0..<infiles.len:
    var ms = ModuleState(outfile: outfiles[i])
    ms.owningBuf = createTokenBuf(300)
    if i == 0:
      ms.n0 = setupProgram(infiles[i], outfiles[i], ms.owningBuf)
      ms.c = initSemContext(prog.main.name, sharedConfig, moduleFlags,
                            commandLineArgs, canSelfExec)
    else:
      let suffix = splitModulePath(infiles[i]).name
      ms.n0 = loadModule(infiles[i], ms.owningBuf, suffix)
      ms.c = initSemContext(suffix, sharedConfig, moduleFlags,
                            commandLineArgs, canSelfExec)
    ms.dest = createTokenBuf()
    modules.add ensureMove ms

  # Setup: create scopes and import system for each module
  for i in 0..<modules.len:
    modules[i].c.currentScope = Scope(tab: initTable[StrId, seq[Sym]](), kind: ToplevelScope)
    let path = getFile(modules[i].n0.info)
    addSelfModuleSym(modules[i].c, path)
    if {SkipSystem, IsSystem} * moduleFlags == {}:
      let systemFile = ImportedFilename(path: stdlibFile("std/system"), name: "system", isSystem: true)
      importSingleFile(modules[i].c, modules[i].dest, systemFile, "", ImportFilter(kind: ImportAll), modules[i].n0.info)

  # Phase 1: Register toplevel symbols for ALL modules
  for i in 0..<modules.len:
    var (buf1, lineInfo) = phase1(modules[i].c, modules[i].dest, modules[i].n0)
    modules[i].buf1 = ensureMove buf1
    modules[i].moduleLineInfo = lineInfo

  # After phase1: resolve deferred cyclic imports from prog.mem
  # (type symbols are published during phase1, not phase2)
  for i in 0..<modules.len:
    if modules[i].c.deferredCyclicImports.len > 0:
      resolveCyclicImports(modules[i].c)

  # Phase 2: Check signatures for ALL modules
  # This publishes proc symbols to prog.mem via publishSignature
  for i in 0..<modules.len:
    var buf2 = phase2(modules[i].c, modules[i].buf1, modules[i].moduleLineInfo)
    modules[i].buf1 = ensureMove buf2

  # After phase2: resolve deferred cyclic imports from prog.mem again
  # (proc symbols are only published during phase2, not phase1)
  for i in 0..<modules.len:
    if modules[i].c.deferredCyclicImports.len > 0:
      resolveCyclicImports(modules[i].c)

  # Phase 3: Check bodies for ALL modules
  for i in 0..<modules.len:
    modules[i].dest = phase3(modules[i].c, modules[i].buf1, modules[i].moduleLineInfo)

  # Post-processing and output for each module
  for i in 0..<modules.len:
    semcheckPostProcess modules[i].c, modules[i].dest
    if reportErrors(modules[i].dest) == 0:
      maybeValidatePostSem modules[i].dest, modules[i].outfile
      writeOutput modules[i].c, modules[i].dest, modules[i].outfile
    else:
      quit 1

proc semcheck*(infiles, outfiles: seq[string]; config: sink NifConfig; moduleFlags: set[ModuleFlag];
               commandLineArgs: sink string; canSelfExec: bool) =
  ## Semantic check one or more modules.
  ## For single modules (len=1), this is the normal case.
  ## For multiple modules, they form a cycle group and are processed together.
  assert infiles.len == outfiles.len
  assert infiles.len > 0

  if infiles.len > 1:
    semcheckCycleGroup(infiles, outfiles, ensureMove config, moduleFlags,
                       commandLineArgs, canSelfExec)
    return

  let infile = infiles[0]
  let outfile = outfiles[0]

  var owningBuf = createTokenBuf(300)
  var n0 = setupProgram(infile, outfile, owningBuf)
  if SkipSystem in moduleFlags:
    programs.publishStringType()
  var c = initSemContext(prog.main.name, ProgramContext(config: config),
                         moduleFlags, commandLineArgs, canSelfExec)

  var dest = createTokenBuf()

  while true:
    semcheckCore c, dest, n0
    if c.pendingTypePlugins.len == 0 and c.pendingModulePlugins.len == 0: break
    handleTypePlugins c, dest

  if reportErrors(dest) == 0:
    maybeValidatePostSem dest, outfile
    writeOutput c, dest, outfile
  else:
    quit 1
