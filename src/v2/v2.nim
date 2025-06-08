#       V2 plugin
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

const
  Usage = """v2 - Nim 2.0 plugin for Nimony

Usage:
  v2 path/file.nim nimcache/modname.nif nimcache/modname.idx.nif
"""

import std/[assertions, os]
include ".." / lib / nifprelude
import ".." / lib / [nifindexes, symparser]
import ".." / nimony / [decls, nimony_model, programs, indexgen]
import ".." / gear2 / modnames

proc main(nimFile, nifFile, idxFile: string) =
  let (nimcacheDir, name, ext) = splitModulePath(nifFile)
  let v2dir = nimcacheDir / "v2"
  createDir v2dir

  let c = "nim nif --nimcache:" & quoteShell(v2dir) & " " & quoteShell(nimFile)
  if os.execShellCmd(c) != 0:
    quit "v2: failed to compile " & nimFile
  moveFile v2dir / name & ".nim2.nif", nifFile
  indexFromNif nifFile

if paramCount() != 3:
  quit "v2: invalid number of arguments; expected 3, got " & $paramCount()
else:
  main(paramStr(1), paramStr(2), paramStr(3))
