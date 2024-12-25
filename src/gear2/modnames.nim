#       Nifler
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

# --------------------------------------------------
# Fast&sufficient string hashing. We use custom code here so that we're independent
# from Nim's hashing implementation. Stability is more important as these checksums
# will end up in every NIF file.

type
  UHash = uint32

proc `!&`(h: UHash; val: uint32): UHash {.inline.} =
  ## Mixes a hash value `h` with `val` to produce a new hash value.
  result = h + val
  result = result + (result shl 10'u32)
  result = result xor (result shr 6'u32)

proc `!$`(h: UHash): UHash {.inline.} =
  ## Finishes the computation of the hash value.
  result = h + h shl 3'u32
  result = result xor (result shr 11'u32)
  result = result + result shl 15'u32

proc uhash(s: string): UHash =
  result = 0'u32
  for c in items(s): result = result !& uint32(c)
  result = !$result

# ------------------------------------------

from std / os import splitFile, relativePath, isAbsolute, getCurrentDir, `/`
from std / strutils import replace

proc extractModulename(x: string): string = splitFile(x).name

const
  PrefixLen = 3 # we need to keep it short because it ends up everywhere in the produced C++ code

const
  Base36 = "0123456789abcdefghijklmnopqrstuvwxyz"

proc moduleSuffix*(path: string; searchPaths: openArray[string]): string =
  var f = relativePath(path, getCurrentDir(), '/')
  # Select the path that is shortest relative to the searchPath:
  for s in searchPaths:
    let candidate = relativePath(path, s, '/')
    if candidate.len < f.len:
      f = candidate
  #when defined(windows): path.replace('\\', '/') else: path
  #pathutils.customPath(path)
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
  echo moduleSuffix("/Users/rumpf/projects/nim/lib/system.nim")
  echo moduleSuffix("/Users/araq/projects/nim/lib/system.nim")
