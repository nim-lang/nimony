#       Nifler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Implements the mapping from Nim AST -> NIF.

when defined(nifBench):
  import std / monotimes

import std / [assertions, syncio, os]

import compiler / [
  ast, modulegraphs, modules, commands, options, pathutils, renderer, lineinfos,
  syntaxes, llstream, idents, msgs, passes, sem, pipelines, reorder, nimeval]

import ".." / lib / nifbuilder
import ".." / models / nifler_tags

proc connectCallbacks(graph: ModuleGraph) =
  graph.includeFileCallback = modules.includeModule
  graph.importModuleCallback = passes.importModule

const
  Paths = [
    "std",
    "deprecated/core",
    "deprecated/pure",
    "pure/collections",
    "pure/concurrency",
    "impure",
    "wrappers",
    "wrappers/linenoise",
    "windows",
    "posix",
    "js",
    "pure/unidecode",
    "arch",
    "core",
    "pure"
  ]

proc semFile*(thisfile, outfile: string; portablePaths, depsEnabled: bool) =
  var conf = newConfigRef()

  conf.libpath = AbsoluteDir nimeval.findNimStdLib()

  conf.searchPaths.add(conf.libpath)
  for p in Paths:
    conf.searchPaths.add(conf.libpath / RelativeDir p)
  conf.cmd = cmdCheck

  var graph = newModuleGraph(newIdentCache(), conf)
  #if not self.loadConfigsAndProcessCmdLine(cache, conf, graph):
  #  return
  connectCallbacks(graph)

  if conf.cmd == cmdCheck and optWasNimscript notin conf.globalOptions and
       conf.backend == backendInvalid:
    conf.backend = backendC

  if conf.selectedGC == gcUnselected:
    if conf.backend in {backendC, backendCpp, backendObjc} or
        (conf.cmd in cmdDocLike and conf.backend != backendJs) or
        conf.cmd == cmdGendepend:
      initOrcDefines(conf)

  var p = default(Parser)
  let fileIdx = fileInfoIdx(conf, AbsoluteFile thisfile)

  var module = newModule(graph, fileIdx)
  module.flags.incl {sfSystemModule}
  registerModule(graph, module)

  prepareConfigNotes(graph, module)
  var idgen = idGeneratorFromModule(module)
  let ctx = preparePContext(graph, module, idgen)
  let bModule = PPassContext(nil)

  var s: PLLStream
  let filename = toFullPathConsiderDirty(graph.config, module.fileIdx)
  s = llStreamOpen(filename, fmRead)
  if s == nil:
    rawMessage(graph.config, errCannotOpenFile, filename.string)
    return
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

