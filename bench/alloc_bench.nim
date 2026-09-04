# Alloc/dealloc stress benchmark for rawAlloc/rawDealloc code-quality
# comparisons between backends:
#
#   nimony n -d:danger alloc_bench.nim                      (native: arkham+nifasm)
#   nimony c -d:danger -d:nimNativeAlloc alloc_bench.nim    (C backend, same allocator)
#
# The workload mixes the allocator's paths: small-cell sizes across many size
# classes, medium sizes, and occasional page-crossing big chunks; frees happen
# in shuffled order (xorshift), so free lists actually get exercised. The
# checksum defeats dead-code elimination and doubles as a cross-backend
# correctness check (both binaries must print the same number).

import std / syncio

const
  MaxLive = 4096       # live pointers at any time
  Rounds = 100
  ChurnOps = 200_000   # random free+alloc pairs per round

var slots: array[MaxLive, pointer]
var rngState: uint64 = 0x9E3779B97F4A7C15u64

proc nextRand(): uint64 {.inline.} =
  var x = rngState
  x = x xor (x shl 13)
  x = x xor (x shr 7)
  x = x xor (x shl 17)
  rngState = x
  result = x

proc main() =
  var checksum = 0u64
  for r in 0 ..< Rounds:
    # fill: varied small sizes (many size classes)
    for i in 0 ..< MaxLive:
      let size = int(nextRand() mod 240u64) + 16
      let p = alloc(size)
      cast[ptr uint8](p)[] = uint8(i and 0xFF)
      slots[i] = p
    # churn: free a pseudo-random live slot, allocate a replacement
    for op in 0 ..< ChurnOps:
      let j = int(nextRand() mod uint64(MaxLive))
      checksum = checksum + uint64(cast[ptr uint8](slots[j])[])
      dealloc(slots[j])
      let size = int(nextRand() mod 1000u64) + 8
      let p = alloc(size)
      cast[ptr uint8](p)[] = uint8(j and 0xFF)
      slots[j] = p
    # big chunks: page-and-above allocations, freed immediately
    for i in 0 ..< 256:
      let size = int(nextRand() mod 60000u64) + 4096
      let p = alloc(size)
      cast[ptr uint8](p)[] = 1u8
      checksum = checksum + 1u64
      dealloc(p)
    # drain: free everything (shuffled enough by the churn phase)
    for i in 0 ..< MaxLive:
      dealloc(slots[i])
  echo cast[int](checksum)

main()
