#       Nifler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import ".." / lib / tinyhashes
from std / os import splitFile, relativePath, isAbsolute, getCurrentDir, `/`

include ".." / lib / compat2

proc extractModulename(x: string): string = splitFile(x).name

const
  PrefixLen = 3 # we need to keep it short because it ends up everywhere in the produced C++ code

const
  Base36 = "0123456789abcdefghijklmnopqrstuvwxyz"

proc uhashBase36*(s: string): string =
  var id = uhash(s)
  result = newStringOfCap(8)
  # Convert decimal number to base 36, reversed since it does not matter:
  while id > 0'u32:
    result.add Base36[int(id mod 36'u32)]
    id = id div 36'u32

const
  ## Name of the `<nimcache>/` subdirectory that holds everything a backend
  ## produces from hexer onward: every module's `.x.nif` and, one level down
  ## in `<tag>/<mainmod>/`, the main-specific DCE output, code, objects and
  ## executable (`deps.backendDirName`). Those artifacts differ per backend --
  ## hexer alone runs with or without `--native` -- while nifmake reruns a node
  ## only when its input or output FILES changed, never when a tool's flags
  ## did, so two backends sharing a directory would reuse each other's
  ## artifacts. The names are the commands' letters. Kept here so `hastur`,
  ## which shells out to the compiler, can name the same directory without
  ## duplicating the mapping.
  BackendDirC* = "c"       ## `nimony c`
  BackendDirLLVM* = "l"    ## `nimony l`
  BackendDirNative* = "n"  ## `nimony n`
  BackendDirWasm* = "w"    ## `nimony w`
  BackendDirJs* = "j"      ## `nimony j`

proc moduleSuffix*(path: string; searchPaths: openArray[string]): string =
  # `getCurrentDir`/`relativePath` are `.raises`, but the only reason they can
  # fail in practice is a transient filesystem error that would break every
  # diagnostic equally. Swallow it here so this helper (widely used inside
  # sem/programs) stays non-raising.
  var f = path
  try:
    f = relativePath(path, getCurrentDir(), '/')
  except:
    discard
  # Select the path that is shortest relative to the searchPath:
  for s in searchPaths:
    try:
      let candidate = relativePath(path, s, '/')
      if candidate.len < f.len:
        f = candidate
    except:
      discard
  let m = extractModulename(f)
  var id = uhash(f)
  result = newStringOfCap(10)
  for i in 0..<min(m.len, PrefixLen):
    result.add m[i]
  # Convert decimal number to base 36, reversed since it does not matter:
  while id > 0'u32:
    result.add Base36[int(id mod 36'u32)]
    id = id div 36'u32

when isMainModule and not defined(nimony):
  #echo moduleSuffix("/Users/rumpf/projects/nim/lib/system.nim")
  #echo moduleSuffix("/Users/araq/projects/nim/lib/system.nim")
  echo moduleSuffix("/Users/rumpf/projects/nimony/lib/std/system.nim", [])
  echo moduleSuffix("/Users/rumpf/projects/nimony/lib/std/system.nim", ["/Users/rumpf/projects/nimony/lib"])
