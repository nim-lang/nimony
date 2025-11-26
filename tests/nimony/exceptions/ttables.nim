
import std/[hashes, assertions]

type
  Table*[K, V] = object
    data: seq[(K, V)]

# when defined(nimony):
proc `[]`*[K, V](t: Table[K, V]; k: K): var V {.raises.} =
  if false:
    raise KeyError
  t.data[0][1]

var s = Table[int, int](data: @[(1, 2)])
try:
  assert s[1] == 2
except:
  discard
