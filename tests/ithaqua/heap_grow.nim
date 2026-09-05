# B0 gate: the wasm heap GROWS on demand (memory.grow provider) — this
# allocates well past the module's initial linear-memory reservation.
# 6 × 8 MB seqs = 48 MB live, touched at both ends to prove the pages are real.
#
# The native oracle does NOT use the standalone heap, and an earlier version of
# this comment said it did — it claimed the 48 MB was sized to stay under "its
# fixed 128 MB standalone heap". `nimony n` defines `nimNativeAlloc` /
# `nimNativeIo`, not `standalone`, so the native leg allocates the ordinary way
# and that constant is unreachable from here. The figure is sized for the WASM
# leg alone; nothing about it is a native constraint.
import std/syncio

when defined(wasm32):
  # Expose the ceiling setter as a MAIN-module root: ithaqua's export roots
  # are main-module exportc procs only, so the host-facing control from
  # osalloc must be pulled through explicitly. Apps that let the host cap
  # the heap (run_heap_ceiling.js, the ward shim) carry this wrapper.
  proc hostSetHeapCeiling(bytes: int32) {.exportc: "nim_set_heap_ceiling".} =
    setWasmHeapCeiling(int(bytes))

const ChunkBytes = 8 * 1024 * 1024

var keep: seq[seq[uint8]] = @[]
var round = 0
while round < 6:
  var chunk = newSeq[uint8](ChunkBytes)
  chunk[0] = uint8(round + 1)
  chunk[ChunkBytes - 1] = uint8(41 + round)
  keep.add chunk
  echo "round ", round, ": first ", keep[round][0],
    " last ", keep[round][ChunkBytes - 1]
  inc round

var total = 0
for c in keep:
  total = total + c.len
echo "total bytes: ", total          # 50331648
