# --------------------------------------------------
# Fast&sufficient string hashing. We use custom code here so that we're independent
# from Nim's hashing implementation. Stability is more important as these checksums
# will end up in every NIF file.

type
  UHash* = uint32

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

proc uhash*(s: string): UHash =
  result = 0'u32
  for c in items(s): result = result !& uint32(c)
  result = !$result
