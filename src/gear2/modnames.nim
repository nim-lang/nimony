#       Nifler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import ".." / lib / tinyhashes
from std / os import splitFile, relativePath, isAbsolute, getCurrentDir, `/`

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

proc moduleSuffix*(path: string; searchPaths: openArray[string]): string =
  var f = relativePath(path, getCurrentDir(), '/')
  # Select the path that is shortest relative to the searchPath:
  for s in searchPaths:
    let candidate = relativePath(path, s, '/')
    if candidate.len < f.len:
      f = candidate
  let m = extractModulename(f)
  var id = uhash(f)
  result = newStringOfCap(10)
  for i in 0..<min(m.len, PrefixLen):
    result.add m[i]
  # Convert decimal number to base 36, reversed since it does not matter:
  while id > 0'u32:
    result.add Base36[int(id mod 36'u32)]
    id = id div 36'u32

when isMainModule:
  #echo moduleSuffix("/Users/rumpf/projects/nim/lib/system.nim")
  #echo moduleSuffix("/Users/araq/projects/nim/lib/system.nim")
  echo moduleSuffix("/Users/rumpf/projects/nimony/lib/std/system.nim", [])
  echo moduleSuffix("/Users/rumpf/projects/nimony/lib/std/system.nim", ["/Users/rumpf/projects/nimony/lib"])
