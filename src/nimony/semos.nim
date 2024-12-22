#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Path handling and `exec` like features as `sem.nim` needs it.

import std / [tables, sets, os, syncio, formatfloat, assertions]
include nifprelude
import nimony_model, symtabs, builtintypes, decls, symparser,
  programs, sigmatch, magics, reporters, nifconfig, nifindexes,
  semdata

import ".." / gear2 / modnames

proc stdFile(f: string): string =
  getAppDir() / "lib" / f

proc resolveFile*(c: SemContext; origin: string; toResolve: string): string =
  let nimFile = toResolve.addFileExt(".nim")
  #if toResolve.startsWith("std/") or toResolve.startsWith("ext/"):
  #  result = stdFile nimFile
  if toResolve.isAbsolute:
    result = nimFile
  else:
    result = splitFile(origin).dir / nimFile
    var i = 0
    while not fileExists(result) and i < c.g.config.paths.len:
      result = c.g.config.paths[i] / nimFile
      inc i

# ------------------ include/import handling ------------------------

proc findTool*(name: string): string =
  let exe = name.addFileExt(ExeExt)
  result = getAppDir() / exe

proc exec*(cmd: string) =
  if execShellCmd(cmd) != 0: quit("FAILURE: " & cmd)

proc parseFile*(nimFile: string; paths: openArray[string]): TokenBuf =
  let nifler = findTool("nifler")
  let name = moduleSuffix(nimFile, paths)
  let src = "nifcache" / name & ".1.nif"
  exec quoteShell(nifler) & " --portablePaths p " & quoteShell(nimFile) & " " &
    quoteShell(src)

  var stream = nifstreams.open(src)
  try:
    discard processDirectives(stream.r)
    result = fromStream(stream)
  finally:
    nifstreams.close(stream)

proc getFile*(c: var SemContext; info: PackedLineInfo): string =
  let (fid, _, _) = unpack(pool.man, info)
  result = pool.files[fid]

proc selfExec*(c: var SemContext; file: string) =
  exec os.getAppFilename() & c.commandLineArgs & " m " & quoteShell(file)
