#       Nifler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Implements nifler's `config` format.

import std/[os, strutils, parseopt, strtabs, times]
from std/sequtils import addUnique

import compiler / [
  commands, options, msgs, idents, lineinfos, cmdlinehelper,
  pathutils, modulegraphs, condsyms]

include ".." / lib / nifprelude

proc nimbleLockExists(config: ConfigRef): bool =
  const nimbleLock = "nimble.lock"
  let pd = if not config.projectPath.isEmpty: config.projectPath else: AbsoluteDir(getCurrentDir())
  if optSkipParentConfigFiles notin config.globalOptions:
    for dir in parentDirs(pd.string, fromRoot=true, inclusive=false):
      if fileExists(dir / nimbleLock):
        return true
  return fileExists(pd.string / nimbleLock)

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

  if config.nimbleLockExists:
    # disable nimble path if nimble.lock is present.
    # see https://github.com/nim-lang/nimble/issues/1004
    disableNimblePath(config)

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

  if optWasNimscript notin conf.globalOptions and
       conf.backend == backendInvalid:
    conf.backend = backendC

  if conf.selectedGC == gcUnselected:
    initOrcDefines(conf)

proc toNifPath(p: AbsoluteDir|AbsoluteFile): string =
  relativePath(p.string, getCurrentDir(), '/')

proc genStringTable(b: var Builder; tag: string; tab: StringTableRef) =
  b.withTree tag:
    for key, val in pairs(tab):
      b.withTree "kv":
        b.addStrLit key
        b.addStrLit val

proc buildConfig(b: var Builder; conf: ConfigRef) =
  b.withTree "defines":
    for def in definedSymbolNames(conf.symbols):
      b.addStrLit def

  var paths: seq[string] = @[]
  for p in conf.searchPaths:
    paths.addUnique p.toNifPath
  when false:
    # these only contain paths derivable from 'nimblepaths':
    for p in conf.lazyPaths:
      paths.addUnique p.toNifPath
  b.withTree "paths":
    for p in paths:
      b.addStrLit p

  b.withTree "nimblepaths":
    for p in conf.nimblePaths:
      b.addStrLit p.toNifPath

  b.withTree "backend":
    b.addStrLit $conf.backend

  b.withTree "gc":
    b.addStrLit toLowerAscii($conf.selectedGC)
  b.withTree "exceptionsystem":
    b.addStrLit toLowerAscii(($conf.exc).substr(3))

  b.withTree "options":
    for opt in conf.options:
      b.addStrLit toLowerAscii(($opt).substr(3))
    for opt in conf.globalOptions:
      b.addStrLit toLowerAscii(($opt).substr(3))

  b.withTree "ccompiler":
    b.addStrLit toLowerAscii(($conf.cCompiler).substr(2))

  b.withTree "target":
    b.withTree "os":
      b.addStrLit toLowerAscii(($conf.target.targetOS).substr(2))
    b.withTree "cpu":
      b.addStrLit toLowerAscii(($conf.target.targetCPU).substr(3))
    b.withTree "intbits":
      b.addIntLit conf.target.intSize*8
    b.withTree "floatbits":
      b.addIntLit conf.target.floatSize*8
    b.withTree "ptrbits":
      b.addIntLit conf.target.ptrSize*8

  b.withTree "host":
    b.withTree "os":
      b.addStrLit toLowerAscii(($conf.target.hostOS).substr(2))
    b.withTree "cpu":
      b.addStrLit toLowerAscii(($conf.target.hostCPU).substr(3))

  b.withTree "dirs":
    b.withTree "prefix":
      b.addStrLit conf.prefixDir.toNifPath
    b.withTree "lib":
      b.addStrLit conf.libpath.toNifPath
    b.withTree "nimcache":
      b.addStrLit conf.nimcacheDir.toNifPath
    b.withTree "output":
      b.addStrLit conf.outDir.toNifPath

  b.withTree "features":
    for feature in conf.features: b.addStrLit $feature
    for feature in conf.legacyFeatures: b.addStrLit $feature

  b.withTree "sources":
    for f in conf.m.fileInfos:
      b.addStrLit f.fullPath.toNifPath

  b.genStringTable "vars", conf.configVars
  b.genStringTable "dlloverrides", conf.dllOverrides
  b.genStringTable "moduleoverrides", conf.moduleOverrides
  b.genStringTable "cfilespecific", conf.cfileSpecificOptions

  b.withTree "outfile":
    b.addStrLit conf.outFile.string

  b.withTree "hints":
    for note in conf.notes:
      if note >= hintMin and note <= hintMax:
        b.addStrLit toLowerAscii(($note))

  b.withTree "warnings":
    for note in conf.notes:
      if note >= warnMin and note <= warnMax:
        b.addStrLit toLowerAscii(($note))

  b.withTree "warningsaserrors":
    for note in conf.warningAsErrors:
      b.addStrLit toLowerAscii(($note))


proc produceConfig*(infile, outfile: string) =
  let conf = newConfigRef()
  conf.lastCmdTime = epochTime()

  let nimExe = findExe"nim"
  if nimExe.len > 0:
    let p = splitPath(splitFile(nimExe).dir).head
    conf.prefixDir = AbsoluteDir(p)

  handleCmdLine(newIdentCache(), conf)

  var b = nifbuilder.open(outfile)
  try:
    b.addHeader()
    b.withTree "config":
      b.buildConfig conf
    if outfile.isAbsolute:
      let p = outfile.splitPath
      conf.outDir = AbsoluteDir(p.head)
      conf.outFile = RelativeFile(p.tail)
    else:
      conf.outDir = AbsoluteDir getCurrentDir()
      conf.outFile = RelativeFile(outfile)

    conf.setNote hintSuccessX
    genSuccessX(conf)
  finally:
    b.close()

proc sourcesChangedImpl(configFile: string; c: Cursor): bool =
  var c = c
  var nested = 0
  let modtime = getLastModificationTime(configFile)
  while true:
    case c.kind
    of ParLe:
      inc nested
      if pool.tags[c.tag] == "sources":
        inc c
        while c.kind != ParRi:
          if c.kind == StringLit:
            let dep = pool.strings[c.litId]
            if not fileExists(dep):
              return true
            if getLastModificationTime(dep) >= modtime:
              return true
          inc c
      else:
        inc c
    of ParRi:
      dec nested
      if nested == 0: break
      inc c
    else:
      inc c
  return false

proc sourcesChanged*(configFile: string): bool =
  var f = nifstreams.open(configFile)
  discard processDirectives(f.r)
  var buf = fromStream(f)
  var c = beginRead(buf)
  try:
    result = sourcesChangedImpl(configFile, c)
  finally:
    f.close()
