#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A fast, non-cryptographic pseudo-random number generator (`xoroshiro128+`).
##
## Each proc has two forms: one taking an explicit `Rand` state, and one using a
## shared default generator. The default-generator procs are **not** thread-safe
## — give each thread its own `Rand` via `initRand`.

type
  Rand* = object ## State of a random number generator. Create one with `initRand`.
    a0, a1: uint64

const randMax = 18446744073709551615'u64 # uint64.high

func rotl(x, k: uint64): uint64 {.inline.} =
  (x shl k) or (x shr (64'u64 - k))

func next*(r: var Rand): uint64 =
  ## Returns the next raw random `uint64` and advances `r`.
  let s0 = r.a0
  var s1 = r.a1
  result = s0 + s1
  s1 = s1 xor s0
  r.a0 = rotl(s0, 55'u64) xor s1 xor (s1 shl 14'u64)
  r.a1 = rotl(s1, 36'u64)

proc skipRandomNumbers*(r: var Rand) =
  ## Advances `r` by 2^64 steps — used to split a generator into non-overlapping
  ## subsequences for parallel work.
  let helper = [0xbeac0467eba5facb'u64, 0xd86b048b86aa9922'u64]
  var s0 = 0'u64
  var s1 = 0'u64
  for i in 0 ..< 2:
    for b in 0 ..< 64:
      if (helper[i] and (1'u64 shl uint64(b))) != 0'u64:
        s0 = s0 xor r.a0
        s1 = s1 xor r.a1
      discard next(r)
  r.a0 = s0
  r.a1 = s1

func randUint64(r: var Rand; max: uint64): uint64 =
  ## Uniform `uint64` in `0 .. max` (inclusive), avoiding modulo bias.
  if max == 0'u64: return 0'u64
  if max == randMax: return next(r)
  var iters = 0
  while true:
    let x = next(r)
    if x <= randMax - (randMax mod max) or iters > 20:
      return x mod (max + 1'u64)
    inc iters

proc initRand*(seed: int64): Rand =
  ## Creates a `Rand` seeded with `seed`. The same seed always yields the same
  ## sequence. `seed == 0` is mapped to a fixed non-zero value.
  let s = if seed != 0: seed else: 2147483647'i64
  result = Rand(a0: uint64(s shr 16), a1: uint64(s and 0xffff))
  skipRandomNumbers(result)
  discard next(result)

# default shared generator
var state = initRand(0x69B4C98C'i64)

proc rand*(r: var Rand; max: Natural): int =
  ## Random integer in `0 .. max` (inclusive) using `r`.
  int(randUint64(r, uint64(max)))

proc rand*(max: int): int =
  ## Random integer in `0 .. max` using the default generator.
  rand(state, max)

proc rand*(r: var Rand; max: float): float =
  ## Random float in `0.0 .. max` using `r`.
  (float(next(r)) / float(randMax)) * max

proc rand*(max: float): float =
  ## Random float in `0.0 .. max` using the default generator.
  rand(state, max)

proc rand*(r: var Rand; x: HSlice[int, int]): int =
  ## Random integer in the inclusive range `x` using `r`.
  int(randUint64(r, uint64(x.b - x.a))) + x.a

proc rand*(x: HSlice[int, int]): int =
  ## Random integer in the inclusive range `x` using the default generator.
  rand(state, x)

proc rand*(r: var Rand; x: HSlice[float, float]): float =
  ## Random float in the inclusive range `x` using `r`.
  rand(r, x.b - x.a) + x.a

proc rand*(x: HSlice[float, float]): float =
  ## Random float in the inclusive range `x` using the default generator.
  rand(state, x)

proc sample*[T](r: var Rand; a: openArray[T]): T =
  ## Returns a random element of `a` using `r`.
  a[rand(r, 0 .. a.len - 1)]

proc sample*[T](a: openArray[T]): T =
  ## Returns a random element of `a` using the default generator.
  sample(state, a)

proc shuffle*[T](r: var Rand; x: var openArray[T]) =
  ## Fisher-Yates shuffle of `x` in place using `r`.
  var i = x.len - 1
  while i >= 1:
    let j = rand(r, 0 .. i)
    swap(x[i], x[j])
    dec i

proc shuffle*[T](x: var openArray[T]) =
  ## Shuffles `x` in place using the default generator.
  shuffle(state, x)

proc randomize*(seed: int64) =
  ## Reseeds the default generator with `seed`.
  state = initRand(seed)
