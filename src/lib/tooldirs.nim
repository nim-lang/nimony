## This module provides functions to find tools in the Nimony bin directory.

import std / [os, strutils]

proc binDir*(): string =
  ## The directory tools live in. `bin*` is matched (not just `bin`) so the
  ## boot bootstrap can stage parallel toolchains under sibling directories
  ## like `bin0`, `bin1`, `bin2` without each stage's nimony falling back to
  ## looking in `bin*/bin`.
  let appDir = getAppDir()
  let (_, tail) = splitPath(appDir)
  if tail.startsWith("bin"):
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
