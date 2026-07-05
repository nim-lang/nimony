# Linear-memory WASM backend test: object field loads/stores at the byte offsets
# `jslayout` computes — the SAME layout core the JS backend uses, emitted here as
# WASM `i32.load`/`i32.store`. The driver (`tmemory.js`) lays a `Point` out in the
# shared memory buffer and passes its byte offset (a pointer) in.

type
  Point = object
    x, y, z: int32

proc dot(p: ptr Point): int32 {.exportc.} =
  # p.x @ offset 0, p.y @ 4, p.z @ 8 — each an `i32.load` at base + jslayout offset.
  result = p.x * p.x + p.y * p.y + p.z * p.z

proc setY(p: ptr Point, v: int32) {.exportc.} =
  # p.y @ offset 4 — an `i32.store`.
  p.y = v

proc manhattan(p: ptr Point): int32 {.exportc.} =
  result = p.x + p.y + p.z
