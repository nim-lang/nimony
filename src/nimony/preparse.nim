#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Parses a program's modules up front, many parser processes at a time.
## `deps.nim` walks the import graph one module after the other and parses
## each one it reaches; one parse at a time made the parser's process start,
## ~150 times over, the whole time before the first semcheck. After this the
## walk finds the modules parsed.
##
## Best effort: a module missed here is parsed by the walk as before, an
## extra one costs a parse nobody reads. So `(when …)` import conditions are
## ignored and `include` files are left to the walk.

import std / [os, sets, syncio, assertions, strutils]
from std / osproc import Process, startProcess, waitForExit, close, poParentStreams
from std / cpuinfo import countProcessors
import semos, nifconfig, nimony_model
import ".." / lib / vfs

include ".." / lib / nifprelude

type
  Parse = object
    nimFile, output: string  ## `output`: the `.p.nif`, its `.p.deps.nif` next to it
    std: bool

proc toParse(config: NifConfig; nimFile: string; std: bool): Parse =
  let dir = if std: config.stdcachePath else: config.nifcachePath
  Parse(nimFile: nimFile, std: std,
        output: dir / moduleSuffix(nimFile, config) & ".p.nif")

proc depsOf(p: Parse): string = p.output.changeFileExt(".deps.nif")

proc isFresh(p: Parse; parser: string): bool =
  ## `deps.execNifler`'s rule for skipping the parser.
  if not (vfsExists(p.output) and vfsExists(p.depsOf)): return false
  var srcTime = vfsMtime(p.nimFile)
  if p.std: srcTime = max(srcTime, vfsMtime(parser))
  result = vfsMtime(p.output) > srcTime and vfsMtime(p.depsOf) > srcTime

proc queueImports(config: NifConfig; p: Parse; seen: var HashSet[string];
                  queue: var seq[Parse]) =
  var buf = loadDepsFile(p.depsOf)
  var n = beginRead(buf)
  if not (n.isTagLit and n.stmtKind == StmtsS): return
  n.into:
    while n.hasMore:
      let k = n.stmtKind
      if k notin {ImportS, FromimportS, ImportexceptS}:
        skip n
        continue
      var x = n
      skip n
      var files: seq[ImportedFilename] = @[]
      var hasError = false
      x.into:
        if x.stmtKind == WhenS: skip x
        if k == ImportS:
          while x.hasMore: importedFilename(x, files, hasError)
        else:
          filenameVal(x, files, hasError, allowAs = true)
          while x.hasMore: skip x
      for f in files:
        if f.plugin.len > 0: continue
        let path = resolveImport(config.paths, p.nimFile, f.path)
        if not path.endsWith(".nif") and not seen.containsOrIncl(path) and
            vfsExists(path):
          queue.add toParse(config, path, inStdcache(config, path))

proc preparse*(config: NifConfig; parser, root: string; withSystem: bool) =
  ## `root` is the main module, which is never a stdlib one.
  if root.endsWith(".nif"):
    # `nimony s` starts from an already parsed `.p.nif` (for example a macro
    # plugin module). There is no Nim source to preparse, and handing the NIF
    # tree to the Nim parser would report its first line info as source text.
    return
  let jobs = if config.parallelBuild > 0: config.parallelBuild
             else: countProcessors()
  var seen = initHashSet[string]()
  var queue = @[toParse(config, root, false)]
  seen.incl root
  if withSystem:
    let sys = stdlibFile("std/system.nim")
    seen.incl sys
    queue.add toParse(config, sys, inStdcache(config, sys))
  var running: seq[Process] = @[]
  var parsing: seq[(Parse, string)] = @[]  # module and temp output of each
  var next = 0
  while next < queue.len or running.len > 0:
    while next < queue.len and running.len < jobs:
      let p = queue[next]
      inc next
      if isFresh(p, parser):
        queueImports config, p, seen, queue
        continue
      # By rename, as `semos.runParser`: builds sharing the stdlib cache
      # parse its files without a lock.
      let tmp = atomicTempPath(p.output) & ".nif"
      var args = parserFlags(p.std)
      args.add "parse"
      args.add p.nimFile
      args.add tmp
      try:
        running.add startProcess(parser, args = args, options = {poParentStreams})
        parsing.add (p, tmp)
      except:
        discard "the walk parses it and reports why that fails"
    if running.len == 0: break
    # the most recent one: they take about equally long
    let last = running.len - 1
    var code = -1
    try:
      code = waitForExit(running[last])
      close running[last]
    except:
      code = -1
    let (p, tmp) = parsing[last]
    running.setLen last
    parsing.setLen last
    if code == 0:
      publishShared tmp, p.output
      publishShared tmp.changeFileExt(".deps.nif"), p.depsOf
      queueImports config, p, seen, queue
    else:
      vfsRemove tmp
      vfsRemove tmp.changeFileExt(".deps.nif")
