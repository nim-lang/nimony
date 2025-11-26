## This module provides functions to find tools in the Nimony bin directory.

import std / [os, strutils]

proc binDir*(): string =
  let appDir = getAppDir()
  let (_, tail) = splitPath(appDir)
  if tail == "bin":
    result = appDir
  else:
    result = appDir / "bin"

proc toolDir*(f: string): string =
  result = binDir() / f

proc findTool*(name: string): string =
  result = name.addFileExt(ExeExt)
  if fileExists(result):
    discard "ok"
  elif not name.isAbsolute:
    let t = toolDir(result)
    if fileExists(t):
      result = t
