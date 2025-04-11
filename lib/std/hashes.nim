type
  Hash* = uint

  Hashable* = concept
    proc hash(a: Self): Hash

proc `!&`*(h: Hash; val: uint): Hash {.inline.} =
  ## Mixes a hash value `h` with `val` to produce a new hash value.
  result = h + val
  result = result + (result shl 10)
  result = result xor (result shr 6)

proc `!$`*(h: Hash): Hash {.inline.} =
  ## Finishes the computation of the hash value.
  result = h + h shl 3
  result = result xor (result shr 11)
  result = result + result shl 15

proc hash*(s: string): Hash =
  result = 0'u
  for c in items(s):
    result = result !& uint(c)
  result = !$result

proc hash*(u: uint): Hash {.inline.} = u
when not defined(nimony):
  proc hash*(x: int): Hash {.inline.} = cast[Hash](x)

proc hash*(x: int64): Hash {.inline.} = cast[Hash](x)
proc hash*(x: int32): Hash {.inline.} = cast[Hash](int x)
proc hash*(x: char): Hash {.inline.} = Hash(x)
proc hash*(x: bool): Hash {.inline.} = Hash(x)
proc hash*[T: enum](x: T): Hash {.inline.} = Hash(x)

#[
proc hash*[T: object](x: T): Hash {.inline.} =
  result = 0'u
  for y in fields(x):
    result = result !& hash(y)
  result = !$result
]#

proc nextTry*(h: Hash; maxHash: int): Hash {.inline.} =
  result = (h + 1'u) and maxHash.uint
