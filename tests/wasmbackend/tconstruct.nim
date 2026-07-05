# Aggregate construction in linear memory: objects and arrays are built into a
# bump-allocated region (the `$hp` global — the WASM analog of the JS backend's
# `allocFixed`), then read/written at the SAME `jslayout` byte offsets. Also
# exercises value-semantic aggregate copy (`memory.copy`) and array indexing
# (whose runtime bounds check is a cross-module call, dispatched to a host import
# the driver provides).

type
  Point = object
    x, y, z: int32

proc makeDist(): int32 {.exportc.} =
  # object construction (oconstr) + field reads
  var p = Point(x: 3'i32, y: 4'i32, z: 12'i32)
  result = p.x * p.x + p.y * p.y + p.z * p.z

proc fieldStores(): int32 {.exportc.} =
  var p = Point(x: 0'i32, y: 0'i32, z: 0'i32)
  p.x = 5'i32
  p.y = 6'i32
  p.z = 7'i32
  result = p.x + p.y + p.z

proc valueCopy(): int32 {.exportc.} =
  # `q = p` copies the bytes (value semantics): mutating q must not touch p.
  var p = Point(x: 1'i32, y: 2'i32, z: 3'i32)
  var q = p
  q.x = 100'i32
  result = p.x + q.x

proc arraySum(): int32 {.exportc.} =
  var a = [10'i32, 20'i32, 30'i32, 40'i32]   # array construction (aconstr)
  result = 0'i32
  var i = 0'i32
  while i < 4'i32:
    result = result + a[i]                    # indexed load at base + i*stride
    i = i + 1'i32
