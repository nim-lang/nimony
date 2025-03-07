when not defined(nimcore):
  {.error: "nimcore MUST be defined for Nim's core tooling".}

import std / [os, times, parseopt, syncio, assertions]
import compiler / [
  llstream, ast, options, msgs, condsyms, idents, platform, reorder,
  modules, pipelineutils, pipelines, packages, modulegraphs, lineinfos, pathutils,
  cmdlinehelper, commands, sem, renderer, syntaxes, parser]

import bridge, modnames

proc compilePipelineModule(graph: ModuleGraph; fileIdx: FileIndex; flags: TSymFlags; fromModule: PSym = nil): PSym

proc moduleSuffix(conf: ConfigRef; fileIdx: FileIndex): string =
  moduleSuffix(toFullPath(conf, fileIdx))

proc toNifFile(conf: ConfigRef; fileIdx: FileIndex): string =
  let dest = moduleSuffix(toFullPath(conf, fileIdx))
  result = conf.nimcacheDir / dest

proc importPipelineModule2(graph: ModuleGraph; s: PSym, fileIdx: FileIndex): PSym =
  # this is called by the semantic checking phase
  assert graph.config != nil
  assert s != nil
  #echo "active for ", toFullPath(graph.config, fileIdx), " ", graph.withinSystem
  result = compilePipelineModule(graph, fileIdx, {}, s)
  graph.addDep(s, fileIdx)
  #if sfSystemModule in result.flags:
  #  localError(result.info, errAttemptToRedefine, result.name.s)
  # restore the notes for outer module:
  graph.config.notes =
    if graph.config.belongsToProjectPackage(s) or isDefined(graph.config, "booting"): graph.config.mainPackageNotes
    else: graph.config.foreignPackageNotes

  if sfMainModule in s.flags or graph.withinSystem:
    let dest = toNifFile(graph.config, result.fileIdx).addFileExt".nif"
    toNif(graph.config, result.ast, dest)
    #echo "Producing ", dest, " for ", result.name.s

proc connectPipelineCallbacks2(graph: ModuleGraph) =
  graph.includeFileCallback = modules.includeModule
  graph.importModuleCallback = importPipelineModule2

proc processPipelineModule(graph: ModuleGraph; module: PSym; idgen: IdGenerator;
                           stream: PLLStream): bool =
  if graph.stopCompile(): return true
  var p = default(Parser)
  prepareConfigNotes(graph, module)
  let ctx = preparePContext(graph, module, idgen)
  let bModule = PPassContext(nil)

  var s: PLLStream
  if stream == nil:
    let filename = toFullPathConsiderDirty(graph.config, module.fileIdx)
    s = llStreamOpen(filename, fmRead)
    if s == nil:
      rawMessage(graph.config, errCannotOpenFile, filename.string)
      return false
  else:
    s = stream
  graph.interactive = false

  syntaxes.openParser(p, module.fileIdx, s, graph.cache, graph.config)

  if not belongsToStdlib(graph, module) or (belongsToStdlib(graph, module) and module.name.s == "distros"):
    # XXX what about caching? no processing then? what if I change the
    # modules to include between compilation runs? we'd need to track that
    # in ROD files. I think we should enable this feature only
    # for the interactive mode.
    if module.name.s != "nimscriptapi":
      processImplicitImports graph, graph.config.implicitImports, nkImportStmt, module, ctx, bModule, idgen
      processImplicitImports graph, graph.config.implicitIncludes, nkIncludeStmt, module, ctx, bModule, idgen

  checkFirstLineIndentation(p)
  var n = parseTopLevelStmt(p)
  var moduleStmts = newNodeI(nkStmtList, n.info)
  if n.kind != nkEmpty:
    # read everything, no streaming possible
    var sl = newNodeI(nkStmtList, n.info)
    sl.add n
    while true:
      var n = parseTopLevelStmt(p)
      if n.kind == nkEmpty: break
      sl.add n
    prePass(ctx, sl)
    if sfReorder in module.flags or codeReordering in graph.config.features:
      sl = reorder(graph, sl, module)
    let semNode = semWithPContext(ctx, sl)
    #echo renderTree(semNode)
    moduleStmts.add semNode

  closeParser(p)
  let finalNode = closePContext(graph, ctx, nil)
  #appendToModule(module, finalNode)
  moduleStmts.add finalNode
  appendToModule(module, moduleStmts)
  result = true

var
  nifDb: RContext

proc compilePipelineModule(graph: ModuleGraph; fileIdx: FileIndex; flags: TSymFlags; fromModule: PSym = nil): PSym =
  var flags = flags
  if fileIdx == graph.config.projectMainIdx2:
    flags.incl sfMainModule

  result = graph.getModule(fileIdx)

  template processModuleAux(moduleStatus) =
    onProcessing(graph, fileIdx, moduleStatus, fromModule = fromModule)
    var s: PLLStream = nil
    if sfMainModule in flags:
      if graph.config.projectIsStdin: s = stdin.llStreamOpen
      elif graph.config.projectIsCmd: s = llStreamOpen(graph.config.cmdInput)
    discard processPipelineModule(graph, result, idGeneratorFromModule(result), s)
  if result == nil:
    result = newModule(graph, fileIdx)
    result.flags.incl flags
    if sfMainModule in flags and not graph.withinSystem:
      bridge.openNifModule nifDb, moduleSuffix(graph.config, graph.config.m.systemFileIdx)
    registerModule(graph, result)
    processModuleAux("import")
    if sfSystemModule in flags:
      graph.systemModule = result
    #  partialInitModule(result, graph, fileIdx, filename)

    if sfMainModule in result.flags or graph.withinSystem:
      let dest = toNifFile(graph.config, result.fileIdx).addFileExt".nif"
      toNif(graph.config, result.ast, dest)
      #echo "Producing ", dest, " for ", result.name.s

proc compilePipelineSystemModule2(graph: ModuleGraph) =
  if graph.systemModule == nil:
    connectPipelineCallbacks2(graph)
    graph.config.m.systemFileIdx = fileInfoIdx(graph.config,
        graph.config.libpath / RelativeFile"system.nim")
    discard graph.compilePipelineModule(graph.config.m.systemFileIdx, {sfSystemModule})

proc compilePipelineProject2(graph: ModuleGraph; projectFileIdx = InvalidFileIdx): PSym =
  connectPipelineCallbacks2(graph)
  let conf = graph.config
  wantMainModule(conf)
  configComplete(graph)

  let systemFileIdx = fileInfoIdx(conf, conf.libpath / RelativeFile"system.nim")
  graph.config.m.systemFileIdx = systemFileIdx

  let projectFile = if projectFileIdx == InvalidFileIdx: conf.projectMainIdx else: projectFileIdx
  conf.projectMainIdx2 = projectFile

  let packSym = getPackage(graph, projectFile)
  graph.config.mainPackageId = packSym.getPackageId
  graph.importStack.add projectFile

  if projectFile == systemFileIdx:
    graph.withinSystem = true
    result = graph.compilePipelineModule(projectFile, {sfMainModule, sfSystemModule})
    graph.withinSystem = false
  else:
    graph.withinSystem = true
    #graph.compilePipelineSystemModule2()
    var sys = newModule(graph, systemFileIdx)
    sys.flags.incl sfSystemModule
    registerModule(graph, sys)
    graph.systemModule = sys
    nifDb = createRContext(graph, graph.cache, sys)
    let suffix = moduleSuffix(toFullPath(conf, systemFileIdx))
    bridge.openSystem nifDb, toNifFile(conf, systemFileIdx), suffix
    loadInterface nifDb, sys, suffix
    #graph.compilePipelineModule(systemFileIdx, {sfMainModule, sfSystemModule})
    graph.withinSystem = false
    result = graph.compilePipelineModule(projectFile, {sfMainModule})

proc commandCheck(graph: ModuleGraph) =
  let conf = graph.config
  conf.setErrorMaxHighMaybe
  defineSymbol(conf.symbols, "nimcheck")
  if optWasNimscript in conf.globalOptions:
    defineSymbol(conf.symbols, "nimscript")
    defineSymbol(conf.symbols, "nimconfig")
  elif conf.backend == backendJs:
    setTarget(conf.target, osJS, cpuJS)
  setPipeLinePass(graph, SemPass)
  createDir conf.nimcacheDir
  let module = compilePipelineProject2(graph)
  when defined(debug):
    echo renderTree(module.ast)
  #let dest = moduleSuffix(toFullPath(conf, module.fileIdx))
  #toNif(conf, module.ast, conf.nimcacheDir / dest.addFileExt".nif")

proc processCmdLine(pass: TCmdLinePass, cmd: string; config: ConfigRef) =
  var p = parseopt.initOptParser(cmd)
  var argsCount = 0

  config.commandLine.setLen 0
    # bugfix: otherwise, config.commandLine ends up duplicated

  while true:
    parseopt.next(p)
    case p.kind
    of cmdEnd: break
    of cmdLongOption, cmdShortOption:
      config.commandLine.add " "
      config.commandLine.addCmdPrefix p.kind
      config.commandLine.add p.key.quoteShell # quoteShell to be future proof
      if p.val.len > 0:
        config.commandLine.add ':'
        config.commandLine.add p.val.quoteShell

      if p.key == "": # `-` was passed to indicate main project is stdin
        p.key = "-"
        if processArgument(pass, p, argsCount, config): break
      else:
        processSwitch(pass, p, config)
    of cmdArgument:
      config.commandLine.add " "
      config.commandLine.add p.key.quoteShell
      if processArgument(pass, p, argsCount, config): break
  if pass == passCmd2:
    if {optRun, optWasNimscript} * config.globalOptions == {} and
        config.arguments.len > 0 and config.cmd notin {cmdTcc, cmdNimscript, cmdCrun}:
      rawMessage(config, errGenerated, errArgsNeedRunOption)

proc mainCommand(graph: ModuleGraph) =
  let conf = graph.config
  conf.lastCmdTime = epochTime()
  conf.searchPaths.add(conf.libpath)
  if conf.cmd == cmdM:
    commandCheck graph
  else:
    rawMessage conf, errGenerated, "only the 'm' command is supported for now"

proc handleCmdLine(cache: IdentCache; conf: ConfigRef) =
  let self = NimProg(
    supportsStdinFile: true,
    processCmdLine: processCmdLine
  )
  self.initDefinesProg(conf, "nim_compiler")
  if paramCount() == 0:
    writeCommandLineUsage(conf)
    return

  self.processCmdLineAndProjectPath(conf)

  var graph = newModuleGraph(cache, conf)
  if not self.loadConfigsAndProcessCmdLine(cache, conf, graph):
    return

  if conf.cmd == cmdCheck and optWasNimscript notin conf.globalOptions and
       conf.backend == backendInvalid:
    conf.backend = backendC

  if conf.selectedGC == gcUnselected:
    if conf.backend in {backendC, backendCpp, backendObjc} or
        (conf.cmd in cmdDocLike and conf.backend != backendJs) or
        conf.cmd == cmdGendepend:
      initOrcDefines(conf)

  mainCommand(graph)

when isMainModule:
  handleCmdLine(newIdentCache(), newConfigRef())
