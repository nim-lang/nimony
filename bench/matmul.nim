#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## matmul — dense matrix multiplication, `float32` and `float64`.
##
## The companion to `nifbench`, and deliberately its opposite. `nifbench`
## measures the instruction mix the *compiler* runs: pointer chasing over a
## flat `uint32` array, hash lookups, small allocations. Nothing there tells
## you what the backends do with a numeric kernel — tight counted loops over
## contiguous float memory, where the whole game is address arithmetic,
## register allocation in the innermost loop, and whether the loop is
## vectorized at all.
##
## Same harness as `nifbench` — best-of-N, integer-only output, one file, no
## corpus — pointed at three kernels per element type:
##
## * `ijk`   — the textbook dot-product form. `B` is walked down a column, one
##             cache line touched per iteration and thrown away. Memory-bound;
##             a backend cannot win this one with codegen, which is exactly
##             what makes it the control. It is also the one number here that
##             is not stable: where the matrices happen to land decides how
##             the strided reads collide in the cache, that is fixed for the
##             life of a process, and best-of-`reps` cannot average it out.
##             Runs of the *same binary* differ by a third. Read `ijk` as a
##             floor, draw codegen conclusions from `ikj` and `tiled`, and if
##             you must compare `ijk` across backends, compare the best of
##             several runs rather than one.
## * `ikj`   — the same arithmetic with the loops interchanged, so `B` and `C`
##             are both streamed contiguously and the `A` element is loop
##             invariant. This is the shape an autovectorizer is supposed to
##             recognize; the `ijk`-to-`ikj` ratio is a direct read on whether
##             it did.
## * `tiled` — `ikj` over `Tile`-sized blocks. The blocking is not here to be
##             fast — `ikj` already streams `B` in a way the hardware
##             prefetcher handles, and tiling loses to it at every size
##             measured so far. It is here because it wraps the *identical*
##             inner loop in two more loop levels, a `min` per level and a
##             trip count of `Tile` instead of `n`, so `ikj`-over-`tiled` is a
##             clean read on what a backend charges for loop overhead and for
##             a vector body it can no longer amortize.
##
## `float32` and `float64` run the identical code over the identical values,
## so the pair is a second, independent read on vectorization: with 256-bit
## registers `float32` fits twice as many lanes per instruction, so a
## vectorizing backend shows the two types diverging and a scalar one shows
## them level.
##
## The kernels are written out once per element type rather than once as a
## generic. That is not a style choice: Nimony type-checks a generic body
## eagerly against its constraint, and `SomeFloat` is not enough to resolve
## `*` — the arithmetic operators are declared per concrete type. Templates
## that take the type as a parameter and emit the procs crash `nimsem`
## (`typeprops.nim(353)`). Duplication also happens to be the honest choice
## for a benchmark: what each backend sees is textually the same loop nest.
##
## Every kernel's result is folded into a checksum that is printed, which is
## what keeps the optimizer from deleting the work — and what makes a run
## comparable across backends. Same kernel, same size: every build must print
## the same checksum or one of them is miscompiling. Across kernels they
## differ by design; `ikj` and `tiled` sum a row in a different order than
## `ijk` does, and floating point addition is not associative.
##
## Usage::
##
##   matmul [options]
##
##   --size:N        matrix order (default 256)
##   --only:SUBSTR   run only benchmarks whose name contains SUBSTR
##   --reps:N        repetitions per benchmark, best is reported (default 3)
##   --csv           emit `name,ns,flops,checksum` instead of a table
##   --list          print the benchmark names and exit

import std / [syncio, monotimes, parseopt, strutils]

proc toInt(s: string; fallback: int): int =
  ## `parseInt` raises, and in Nimony that is viral: leaving it uncaught would
  ## push `.raises` onto `main` and everything it touches. Caught at the leaf.
  try: result = parseInt(s)
  except: result = fallback

const
  Version = "0.1.0"
  Tile = 32
    ## Chosen so the three live blocks fit L1 together: at `float64` that is
    ## 3 * 32 * 32 * 8 = 24 KB against a 32 KB L1d. Deliberately NOT tuned per
    ## element type — the tiled kernel is a fixed control-flow shape that the
    ## three backends are compared on, and a tile size that moved with the
    ## type would make the two rows incomparable.
  DefaultSize = 256
    ## 2*256^3 = 33.5 MFLOP per call: long enough that one iteration is far
    ## past timer noise, small enough that the three `float64` matrices
    ## (1.5 MB) do not turn the tiled kernel into a DRAM benchmark.

# ── deterministic pseudo-randomness ─────────────────────────────────────────
# The same 64-bit LCG (Knuth's MMIX constants) `nifbench` uses, for the same
# reason: a run has to be reproducible on any machine without carrying data
# around, and the generator has to be too cheap to matter.

type Gen = object
  s: uint64

proc nextInt(g: var Gen): int =
  g.s = g.s * 6364136223846793005'u64 + 1442695040888963407'u64
  result = int((g.s shr 33'u64) and 0x7FFF_FFFF'u64)

proc below(g: var Gen; n: int): int = g.nextInt() mod n

# ── float64 kernels ─────────────────────────────────────────────────────────

proc mmIjk64(c: var seq[float64]; a, b: seq[float64]; n: int) =
  ## Accumulates into a local rather than into `c[i*n+j]`, so the inner loop
  ## carries the sum in a register and `c` is written once per element. The
  ## strided read of `b` is the whole cost.
  for i in 0 ..< n:
    let ai = i * n
    for j in 0 ..< n:
      var s = 0.0
      for k in 0 ..< n:
        s = s + a[ai + k] * b[k * n + j]
      c[ai + j] = s

proc mmIkj64(c: var seq[float64]; a, b: seq[float64]; n: int) =
  ## Zeroes `c` itself — the inner loop is a read-modify-write, so it needs a
  ## known starting state, and O(n^2) of zeroing against O(n^3) of multiplying
  ## is under half a percent at `--size:256`. Timing it is cheaper than
  ## explaining an untimed prologue.
  for i in 0 ..< n * n: c[i] = 0.0
  for i in 0 ..< n:
    let ai = i * n
    for k in 0 ..< n:
      let av = a[ai + k]
      let bk = k * n
      for j in 0 ..< n:
        c[ai + j] = c[ai + j] + av * b[bk + j]

proc mmTiled64(c: var seq[float64]; a, b: seq[float64]; n: int) =
  ## `while` rather than a `countup` with a step, so the loop bounds are the
  ## same three integer adds in every backend and the comparison is not
  ## measuring somebody's iterator lowering.
  for i in 0 ..< n * n: c[i] = 0.0
  var ii = 0
  while ii < n:
    let iMax = min(ii + Tile, n)
    var kk = 0
    while kk < n:
      let kMax = min(kk + Tile, n)
      var jj = 0
      while jj < n:
        let jMax = min(jj + Tile, n)
        for i in ii ..< iMax:
          let ai = i * n
          for k in kk ..< kMax:
            let av = a[ai + k]
            let bk = k * n
            for j in jj ..< jMax:
              c[ai + j] = c[ai + j] + av * b[bk + j]
        jj = jj + Tile
      kk = kk + Tile
    ii = ii + Tile

proc fill64(s: var seq[float64]; g: var Gen) =
  # Values in [0, 1): the entries of the product then average n/4, so at any
  # size worth running the checksum stays far inside `float32`'s 24-bit
  # mantissa and the two element types remain comparable.
  for i in 0 ..< s.len:
    s[i] = float64(g.below(65536)) * (1.0 / 65536.0)

proc digest64(s: seq[float64]; n: int): int64 =
  ## The mean entry, in thousandths, truncated. Normalized by `n*n` so the
  ## number stays the same order of magnitude at every `--size`, and integral
  ## so it renders identically under three runtimes — `--size:256` on this
  ## data lands near 64000 whatever the backend.
  var acc = 0.0
  for i in 0 ..< n * n: acc = acc + s[i]
  result = int64(acc * 1000.0 / float64(n * n))

# ── float32 kernels ─────────────────────────────────────────────────────────
# Character-for-character the `float64` ones with the type swapped. See the
# module doc for why this is not a generic.

proc mmIjk32(c: var seq[float32]; a, b: seq[float32]; n: int) =
  for i in 0 ..< n:
    let ai = i * n
    for j in 0 ..< n:
      var s = 0.0'f32
      for k in 0 ..< n:
        s = s + a[ai + k] * b[k * n + j]
      c[ai + j] = s

proc mmIkj32(c: var seq[float32]; a, b: seq[float32]; n: int) =
  for i in 0 ..< n * n: c[i] = 0.0'f32
  for i in 0 ..< n:
    let ai = i * n
    for k in 0 ..< n:
      let av = a[ai + k]
      let bk = k * n
      for j in 0 ..< n:
        c[ai + j] = c[ai + j] + av * b[bk + j]

proc mmTiled32(c: var seq[float32]; a, b: seq[float32]; n: int) =
  for i in 0 ..< n * n: c[i] = 0.0'f32
  var ii = 0
  while ii < n:
    let iMax = min(ii + Tile, n)
    var kk = 0
    while kk < n:
      let kMax = min(kk + Tile, n)
      var jj = 0
      while jj < n:
        let jMax = min(jj + Tile, n)
        for i in ii ..< iMax:
          let ai = i * n
          for k in kk ..< kMax:
            let av = a[ai + k]
            let bk = k * n
            for j in jj ..< jMax:
              c[ai + j] = c[ai + j] + av * b[bk + j]
        jj = jj + Tile
      kk = kk + Tile
    ii = ii + Tile

proc fill32(s: var seq[float32]; g: var Gen) =
  for i in 0 ..< s.len:
    s[i] = float32(g.below(65536)) * (1.0'f32 / 65536.0'f32)

proc digest32(s: seq[float32]; n: int): int64 =
  # Accumulated in `float64` on purpose. The kernels are the thing under test;
  # summing 65536 `float32` entries in `float32` would lose the low digits to
  # the checksum's own rounding and hide a real difference between backends.
  var acc = 0.0
  for i in 0 ..< n * n: acc = acc + float64(s[i])
  result = int64(acc * 1000.0 / float64(n * n))

# ── harness ─────────────────────────────────────────────────────────────────

type Result = object
  name: string
  ns: int64
  flops: int64
  digest: int64

var
  results: seq[Result] = @[]
  only = ""
  reps = 3
  curDigest: int64 = 0
    ## Written by each benchmark body, read by `measure` after the timed loop.
    ## A template that took the checksum as a second block would be tidier and
    ## would not survive contact with three front ends.

proc wanted(name: string): bool =
  # `only in name` resolves to the openArray overloads in Nimony; `find` is
  # unambiguous in both compilers.
  only.len == 0 or find(name, only) >= 0

proc report(name: string; ns, flops, digest: int64) =
  results.add Result(name: name, ns: ns, flops: flops, digest: digest)

template measure(name: string; flops: int64; body: untyped) =
  ## Best-of-`reps`. Best rather than mean: the distribution is one-sided —
  ## preemption, page faults and turbo decay only ever make a run slower — so
  ## the minimum is the cleanest estimate of the code's own cost.
  if wanted(name):
    var best = high(int64)
    for r in 0 ..< reps:
      let t0 = getMonoTime().ticks
      body
      let dt = getMonoTime().ticks - t0
      if dt < best: best = dt
    report(name, best, flops, curDigest)

# ── output ──────────────────────────────────────────────────────────────────

proc hundredths(v: int64): string =
  ## `v` is a count of hundredths; render it as `d.dd`. Integer-only, because
  ## `std/formatfloat` bottoms out in `snprintf` and a libc-free `nimony n`
  ## image has no libc to link it from — and because a benchmark whose numbers
  ## are rounded by three different runtimes is a benchmark you cannot diff.
  let whole = v div 100
  let frac = v mod 100
  result = $whole & "." & (if frac < 10: "0" else: "") & $frac

proc fmtRate(flops, ns: int64): string =
  # MFLOP/s = flops / ns * 1000, to two decimals. `flops * 100_000` is 2.1e14
  # at `--size:1024`, four orders below int64's range.
  if ns <= 0 or flops <= 0: return ""
  result = hundredths((flops * 100_000) div ns) & " MFLOP/s"

proc pad(s: string; w: int): string = s & repeat(' ', max(1, w - s.len))

proc printTable(csv: bool) =
  if csv:
    echo "name,ns,flops,checksum"
    for r in results:
      echo r.name, ",", r.ns, ",", r.flops, ",", r.digest
  else:
    echo "benchmark       time            throughput        checksum"
    echo "--------------- --------------- ----------------- ---------"
    for r in results:
      let ms = hundredths(r.ns div 10_000) & " ms"
      echo pad(r.name, 16) & pad(ms, 16) & pad(fmtRate(r.flops, r.ns), 18) &
           $r.digest

# ── main ────────────────────────────────────────────────────────────────────

const BenchNames = ["f64-ijk", "f64-ikj", "f64-tiled",
                    "f32-ijk", "f32-ikj", "f32-tiled"]

proc main =
  var n = DefaultSize
  var csv = false
  for kind, key, val in getopt():
    case kind
    of cmdArgument: quit "unexpected argument: " & key
    of cmdLongOption, cmdShortOption:
      case key
      of "size": n = max(1, toInt(val, DefaultSize))
      of "only": only = val
      of "reps": reps = max(1, toInt(val, 3))
      of "csv": csv = true
      of "list":
        for b in BenchNames: echo b
        return
      of "version", "v":
        echo "matmul ", Version
        return
      of "help", "h":
        echo "matmul [--size:N] [--only:S] [--reps:N] [--csv] [--list]"
        return
      else: quit "unknown option: " & key
    of cmdEnd: discard

  let flops = 2'i64 * int64(n) * int64(n) * int64(n)
  echo "matmul: ", n, "x", n, ", ", flops div 1_000_000, " MFLOP per call, reps=",
       reps, ", tile=", Tile

  # Allocated once and reused across every repetition: what is under test is
  # the kernel, not the allocator, and a fresh 1.5 MB matrix per rep would
  # measure page faults on the first touch of each one.
  var g = Gen(s: 0x9E3779B97F4A7C15'u64)
  var a64 = newSeq[float64](n * n)
  var b64 = newSeq[float64](n * n)
  var c64 = newSeq[float64](n * n)
  fill64(a64, g)
  fill64(b64, g)

  measure "f64-ijk", flops:
    mmIjk64(c64, a64, b64, n)
    curDigest = digest64(c64, n)
  measure "f64-ikj", flops:
    mmIkj64(c64, a64, b64, n)
    curDigest = digest64(c64, n)
  measure "f64-tiled", flops:
    mmTiled64(c64, a64, b64, n)
    curDigest = digest64(c64, n)

  # A fresh generator seeded identically, so `a32`/`b32` hold the `float32`
  # roundings of the very same values `a64`/`b64` do. The two element types
  # are then multiplying the same matrix, and their checksums are supposed to
  # agree to about six digits.
  g = Gen(s: 0x9E3779B97F4A7C15'u64)
  var a32 = newSeq[float32](n * n)
  var b32 = newSeq[float32](n * n)
  var c32 = newSeq[float32](n * n)
  fill32(a32, g)
  fill32(b32, g)

  measure "f32-ijk", flops:
    mmIjk32(c32, a32, b32, n)
    curDigest = digest32(c32, n)
  measure "f32-ikj", flops:
    mmIkj32(c32, a32, b32, n)
    curDigest = digest32(c32, n)
  measure "f32-tiled", flops:
    mmTiled32(c32, a32, b32, n)
    curDigest = digest32(c32, n)

  printTable(csv)

main()
