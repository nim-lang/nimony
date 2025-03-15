#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Path handling and `exec` like features as `sem.nim` needs it.

from std / strutils import multiReplace, split, strip
import std / [tables, sets, os, syncio, formatfloat, assertions]
include nifprelude
import ".." / lib / nifchecksums

import nimony_model, symtabs, builtintypes, decls, symparser, asthelpers,
  programs, sigmatch, magics, reporters, nifconfig, nifindexes,
  semdata

import ".." / gear2 / modnames

proc stdlibDir*(): string =
  let appDir = getAppDir()
  let (head, tail) = splitPath(appDir)
  if tail == "bin":
    result = head / "lib"
  else:
    result = appDir / "lib"

proc setupPaths*(config: var NifConfig; useEnv: bool) =
  if useEnv:
    let nimPath = getEnv("NIMPATH")
    for entry in split(nimPath, PathSep):
      if entry.strip != "":
        config.paths.add entry
    if config.paths.len == 0:
      config.paths.add stdlibDir()
  else:
    config.paths.add stdlibDir()

proc stdlibFile*(f: string): string =
  result = stdlibDir() / f

proc compilerDir*(): string =
  let appDir = getAppDir()
  let (head, tail) = splitPath(appDir)
  if tail == "bin":
    return head
  else: return tail

proc binDir*(): string =
  let appDir = getAppDir()
  let (_, tail) = splitPath(appDir)
  if tail == "bin":
    result = appDir
  else:
    result = appDir / "bin"

proc toolDir*(f: string): string =
  result = binDir() / f

proc absoluteParentDir*(f: string): string =
  result = f.absolutePath().parentDir()

proc fileExists*(f: string): bool =
  result = os.fileExists(f)

proc toAbsolutePath*(f: string): string =
  if f.isAbsolute: return f
  result = os.absolutePath(f)

proc toAbsolutePath*(f: string, dir: string): string =
  if f.isAbsolute: return f
  result = normalizedPath(dir / f)

proc toRelativePath*(f: string, dir: string): string =
  if not f.isAbsolute: return f
  result = f.relativePath(dir)

proc findTool*(name: string): string =
  assert not name.isAbsolute
  let exe = name.addFileExt(ExeExt)
  result = toolDir(exe)

proc exec*(cmd: string) =
  if execShellCmd(cmd) != 0: quit("FAILURE: " & cmd)

proc nimexec(cmd: string) =
  let t = findExe("nim")
  if t.len == 0:
    quit("FAILURE: cannot find nim.exe / nim binary")
  exec quoteShell(t) & " " & cmd

proc updateCompilerGitSubmodules*(config: NifConfig) =
  # XXX: hack for more convenient development
  setCurrentDir compilerDir()
  exec "git submodule update --init"
  setCurrentDir config.currentPath

proc requiresTool*(tool, src: string; forceRebuild: bool) =
  let t = findTool(tool)
  # XXX: hack for more convenient development
  if not fileExists(t) or forceRebuild:
    let src = compilerDir() / src
    let args = # compiler bin path
      when not defined(debug):
        "c -d:release --outdir:" & binDir()
      else: "c --outdir:" & binDir()
    # compile required tool
    nimexec(args & "  " & src)

proc resolveFile*(paths: openArray[string]; origin: string; toResolve: string): string =
  let nimFile = toResolve.addFileExt(".nim")
  #if toResolve.startsWith("std/") or toResolve.startsWith("ext/"):
  #  result = stdFile nimFile
  if toResolve.isAbsolute:
    result = nimFile
  else:
    result = splitFile(origin).dir / nimFile
    var i = 0
    while not fileExists(result) and i < paths.len:
      result = paths[i] / nimFile
      inc i

type ImportedFilename* = object
  path*: string ## stringified path from AST that has to be resolved
  name*: string ## extracted module name to define a sym for in `import`
  isSystem*: bool

proc filenameVal*(n: var Cursor; res: var seq[ImportedFilename]; hasError: var bool; allowAs = false) =
  case n.kind
  of StringLit, Ident:
    let s = pool.strings[n.litId]
    # XXX `s` could be something like "foo/bar.nim" which would need to extract the name "bar"
    res.add ImportedFilename(path: s, name: s)
    inc n
  of Symbol:
    var s = pool.syms[n.symId]
    extractBasename s
    res.add ImportedFilename(path: s, name: s)
    inc n
  of ParLe:
    case exprKind(n)
    of OchoiceX, CchoiceX:
      inc n
      if n.kind != ParRi:
        filenameVal(n, res, hasError)
        while n.kind != ParRi: skip n
        inc n
      else:
        hasError = true
        inc n
    of QuotedX:
      let s = pool.strings[unquote(n)]
      res.add ImportedFilename(path: s, name: s)
    of CallX, InfixX:
      var x = n
      skip n # ensure we skipped it completely
      inc x
      var op = ""
      let opId = getIdent(x)
      if opId == StrId(0):
        hasError = true
      else:
        op = pool.strings[opId]
      if hasError:
        discard
      elif op == "as":
        if not allowAs:
          hasError = true
          return
        if x.kind == ParRi:
          hasError = true
          return
        var rhs = x
        skip rhs # skip lhs
        if rhs.kind == ParRi:
          hasError = true
          return
        let aliasId = getIdent(rhs)
        if aliasId == StrId(0):
          hasError = true
        else:
          let alias = pool.strings[aliasId]
          var prefix: seq[ImportedFilename] = @[]
          filenameVal(x, prefix, hasError, allowAs = false)
          if x.kind != ParRi: hasError = true
          for pre in mitems(prefix):
            if pre.path != "":
              res.add ImportedFilename(path: pre.path, name: alias)
          if prefix.len == 0:
            hasError = true
      else: # any operator, could restrict to slash-like
        var prefix: seq[ImportedFilename] = @[]
        filenameVal(x, prefix, hasError, allowAs = false)
        var suffix: seq[ImportedFilename] = @[]
        filenameVal(x, suffix, hasError, allowAs = allowAs)
        if x.kind != ParRi: hasError = true
        for pre in mitems(prefix):
          for suf in mitems(suffix):
            if pre.path != "" and suf.path != "":
              res.add ImportedFilename(path: pre.path & op & suf.path, name: suf.name)
            else:
              hasError = true
        if prefix.len == 0 or suffix.len == 0:
          hasError = true
    of PrefixX:
      var x = n
      skip n # ensure we skipped it completely
      inc x
      var op = ""
      let opId = getIdent(x)
      if opId == StrId(0):
        hasError = true
      else:
        op = pool.strings[opId]
      if hasError:
        discard
      else: # any operator, could restrict to slash-like
        var suffix: seq[ImportedFilename] = @[]
        filenameVal(x, suffix, hasError, allowAs = allowAs)
        if x.kind != ParRi: hasError = true
        for suf in mitems(suffix):
          if suf.path != "":
            res.add ImportedFilename(path: op & suf.path, name: suf.name)
          else:
            hasError = true
        if suffix.len == 0:
          hasError = true
    of ParX, TupX, BracketX:
      inc n
      if n.kind != ParRi:
        while n.kind != ParRi:
          filenameVal(n, res, hasError, allowAs)
        inc n
      else:
        hasError = true
        inc n
    of AconstrX, TupConstrX:
      inc n
      skip n # skip type
      if n.kind != ParRi:
        while n.kind != ParRi:
          filenameVal(n, res, hasError, allowAs)
        inc n
      else:
        hasError = true
        inc n
    else:
      skip n
      hasError = true
  else:
    skip n
    hasError = true

proc replaceSubs*(fmt, currentFile: string; config: NifConfig): string =
  # Unpack Current File to Absolute
  let nifcache = config.nifcachePath
  var path = absolutePath(currentFile)
  if os.fileExists(path):
    path = parentDir(path)
  # Replace matches with paths
  path = fmt.multiReplace(
    ("${path}", path),
    ("${nifcache}", nifcache))
  result = path.normalizedPath()

# ------------------ include/import handling ------------------------

proc parseFile*(nimFile: string; paths: openArray[string], nifcachePath: string): TokenBuf =
  let nifler = findTool("nifler")
  let name = moduleSuffix(nimFile, paths)
  let src = nifcachePath / name & ".1.nif"
  exec quoteShell(nifler) & " --portablePaths --deps parse " & quoteShell(nimFile) & " " &
    quoteShell(src)

  var stream = nifstreams.open(src)
  try:
    discard processDirectives(stream.r)
    result = fromStream(stream)
  finally:
    nifstreams.close(stream)

proc getFile*(info: PackedLineInfo): string =
  let fid = unpack(pool.man, info).file
  if fid.isValid:
    result = pool.files[fid]
  else:
    result = ""

proc selfExec*(c: var SemContext; file: string; moreArgs: string) =
  exec os.getAppFilename() & c.commandLineArgs & moreArgs & " --ischild m " & quoteShell(file)

# ------------------ plugin handling --------------------------

proc compilePlugin(c: var SemContext; info: PackedLineInfo; nimfile, exefile: string) =
  let nf = resolveFile(c.g.config.paths, getFile(info), nimfile)
  let cmd = "nim c -o " & quoteShell(exefile) & " " & quoteShell(nf)
  exec cmd

proc runPlugin*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo; pluginName, input: string) =
  let p = splitFile(pluginName)
  let basename = c.g.config.nifcachePath / p.name & "_" & computeChecksum(input)
  let inputFile = basename & ".in.nif"
  let outputFile = basename & ".out.nif"
  let pluginExe = c.g.config.nifcachePath / p.name.addFileExt(ExeExt)
  if not fileExists(pluginExe):
    compilePlugin(c, info, pluginName, pluginExe)
  if fileExists(inputFile) and readFile(inputFile) == input:
    # do not touch the timestamp
    discard "nothing to do here"
  else:
    writeFile inputFile, input

  if needsRecompile(pluginExe, outputFile):
    let cmd = quoteShell(pluginExe) & " " & quoteShell(inputFile) & " " & quoteShell(outputFile)
    exec cmd
  var s = nifstreams.open(outputFile)
  try:
    parse s, dest, NoLineInfo
  finally:
    close s
