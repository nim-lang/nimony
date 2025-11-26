#       V2 plugin
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

const
  Usage = """v2 - Nim 2.0 plugin for Nimony

Usage:
  v2 path/file.nim nimcache/modname.nif
"""

import std/[assertions, os, osproc, strutils]
include ".." / lib / nifprelude
import ".." / lib / [nifindexes, symparser, tooldirs]
import ".." / nimony / [programs]
import ".." / gear2 / modnames

proc processDir(dir: string) =
  var nifFiles: seq[string] = @[]
  for kind, path in walkDir(dir):
    if kind == pcFile and path.endsWith(".nim2.nif"):
      nifFiles.add(path)
  for nifFile in mitems nifFiles:
    let newName = nifFile.replace(".nim2.nif", ".2.nif")
    moveFile nifFile, newName
    nifFile = newName
  var cmds: seq[string] = @[]
  let nimsem = findTool("nimsem")
  for newName in nifFiles:
    cmds.add nimsem & " x " & quoteShell(newName)
  if execProcesses(cmds) != 0:
    quit "v2: failed to generate indexes"

proc main(nimFile, nifFile: string) =
  let (v2dir, name, ext) = splitModulePath(nifFile)
  createDir v2dir

  let c = "nim nif --nimcache:" & quoteShell(v2dir) & " " & quoteShell(nimFile)
  if os.execShellCmd(c) != 0:
    quit "v2: failed to compile " & nimFile
  processDir v2dir

if paramCount() != 2:
  quit "v2: invalid number of arguments; expected 2, got " & $paramCount()
else:
  main(paramStr(1), paramStr(2))
