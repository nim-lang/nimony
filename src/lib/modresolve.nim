#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Turning an `import`/`include` path expression into a file on disk.
##
## Shared between the compiler front-end (`nimony/semos.nim`, `nimony/deps.nim`)
## and `nifler scan`, which walks the same module graph ahead of the driver.
## Both must agree on where a module lives, so the rule lives here once.

import std / [os, envvars, strutils]

proc resolveFile*(paths: openArray[string]; origin: string; toResolve: string): string =
  let nimFile = toResolve.addFileExt(".nim")
  if toResolve.isAbsolute:
    result = nimFile
  elif toResolve.len > 0 and toResolve[0] == '$':
    var key = ""
    var i = 1
    while i < toResolve.len:
      if toResolve[i] in {'/', '\\'}:
        break
      key.add toResolve[i]
      inc i
    let val = getEnv(key)
    if val.len == 0:
      result = nimFile
    else:
      result = val / nimFile.substr(i)
  else:
    result = splitFile(origin).dir / nimFile
    var i = 0
    while not fileExists(result) and i < paths.len:
      result = paths[i] / nimFile
      inc i

proc resolveFileWrapper*(paths: openArray[string]; origin: string; toResolve: string): string =
  ## `resolveFile` plus the `std/` fallback: `std/foo` also resolves to a plain
  ## `foo.nim` on the search path, so that stdlib modules can be imported both
  ## ways.
  result = resolveFile(paths, origin, toResolve)
  if not fileExists(result) and toResolve.startsWith("std/"):
    result = resolveFile(paths, origin, toResolve.substr(4))
