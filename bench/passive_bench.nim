#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## passive_bench — the cost of a `.passive` call chain, leg by leg, and a
## markdown table either way.
##
## The `feature/contextvars` branch changed the shape of every coroutine it
## compiles: the CPS pass now hands a hidden context argument to every spawn,
## `CoroutineBase` grew a `ctx` field, and `runStep` saves and restores the
## `runningCoro` thread variable around each `c.fn(c.env)`. This file never
## names a `ContextVar` and does not import `std/contextvars` (which does not
## exist before the feature), so every difference it reports is the tax that
## support puts on ordinary `.passive` code.
##
## ## Two modes
##
## Bench mode (the default) times the kernels in this process and prints a
## markdown table::
##
##   nimony c -r --opt:speed -d:danger bench/passive_bench.nim
##
## Compare mode builds *this same source* with two toolchains — one carrying
## the feature, one from before it — runs each, and prints one markdown table
## with the two toolchains side by side::
##
##   bench/passive_bench --compare --cpu:2
##   bench/passive_bench --compare --feature:bin --baseline:../nimony-master/bin
##
## Compare mode shells out to two `nimony` executables and runs the programs
## they produce; nothing else. There is no shell script and no `find`: the
## binary path is pinned with `-o:`, and the temporary caches are removed
## recursively from Nimony.
##
## ## The kernels
##
## * `reg3`    — reg → reg → reg: no coroutine at all, the floor, and the
##               control for plain codegen drift between the two toolchains.
## * `pas1`    — reg → pas: one passive frame, stack-allocated, driven by
##               `complete`.
## * `pas2`    — reg → pas → pas: the inner call is a suspension point, so the
##               callee's frame is heap-allocated. This is the `.passive ->
##               .passive` leg.
## * `pas2reg` — reg → pas → reg → pas: the outer `.passive` calls an ordinary
##               proc — not a suspension point — and that ordinary proc calls a
##               `.passive` one, which `complete` drives through a nested
##               trampoline with a `Join`. This is the `.passive -> original ->
##               .passive` leg.
## * `pas4`    — reg → pas → pas → pas → pas: the passive-to-passive leg three
##               deep, so the per-leg slope reads off against `pas2`.
##
## ## Why everything mutable is a local
##
## A module that contains a `.passive` proc is coroutine-capable, and the
## feature compiler refuses unsynchronised access to a global in such a module —
## a diagnostic the pre-feature compiler does not have. A benchmark that must
## build under both therefore cannot hold a single global, so each proc owns its
## accumulators and `timed` writes back into the caller's own locals.

import std / [syncio, monotimes, parseopt, strutils, osproc, os, dirs, paths]

const
  smoke = defined(benchSmoke)
    ## `hastur` compiles `bench/` with `-d:benchSmoke` (see `bench/hastur.mode`).
    ## The suite asks whether every kernel still builds and still folds the same
    ## number into its accumulator, not how fast it runs, so a smoke build drops
    ## to one repetition of a small count and prints the per-kernel checksums
    ## alone: those are a golden a diff can hold, the timings are not.

  Version = "0.2.0"
  DefaultCalls = when smoke: 2_000 else: 2_000_000
    ## Long enough that a few-nanosecond-per-call difference between two
    ## toolchains clears timer noise, short enough that five kernels at
    ## best-of-`reps` still finish in about a second.
  DefaultReps = when smoke: 1 else: 5
    ## Repetitions inside one run; the best is kept.
  DefaultRuns = when smoke: 1 else: 5
    ## Whole-program runs per toolchain in compare mode; the best is kept.

  KernelNames = ["reg3", "pas1", "pas2", "pas2reg", "pas4"]

# ── the kernels ─────────────────────────────────────────────────────────────
# Every proc adds one to its argument, so a chain of `n` of them returns
# `x + n`. The regular ones are `noinline` so their call is a real call:
# otherwise the regular side would be spliced away and the coroutine machinery
# would be priced against nothing.

proc rLeaf(x: int): int {.noinline.} = x + 1
proc rMid(x: int): int {.noinline.} = rLeaf(x) + 1
proc rTop(x: int): int {.noinline.} = rMid(x) + 1

proc pLeaf(x: int): int {.passive.} = x + 1
proc pTop(x: int): int {.passive.} = pLeaf(x) + 1

proc rBridge(x: int): int {.noinline.} = pLeaf(x) + 1
proc pBridge(x: int): int {.passive.} = rBridge(x) + 1

proc pChain3(x: int): int {.passive.} = pLeaf(x) + 1
proc pChain2(x: int): int {.passive.} = pChain3(x) + 1
proc pChain(x: int): int {.passive.} = pChain2(x) + 1

# ── drivers ─────────────────────────────────────────────────────────────────
# The loops live in procs rather than in the `timed` template so the template
# introduces no state, and each driver returns the value it accumulated.

proc runReg3(n: int): int =
  var acc = 0
  for i in 0 ..< n: acc = acc + rTop(i)
  result = acc

proc runPas1(n: int): int =
  var acc = 0
  for i in 0 ..< n: acc = acc + pLeaf(i)
  result = acc

proc runPas2(n: int): int =
  var acc = 0
  for i in 0 ..< n: acc = acc + pTop(i)
  result = acc

proc runPas2Reg(n: int): int =
  var acc = 0
  for i in 0 ..< n: acc = acc + pBridge(i)
  result = acc

proc runPas4(n: int): int =
  var acc = 0
  for i in 0 ..< n: acc = acc + pChain(i)
  result = acc

# ── benchmark harness ───────────────────────────────────────────────────────

type Result = object
  name: string
  ns: int64
  calls: int
  digest: int64

template timed(reps: int; curDigest, digestOut, nsOut: untyped; body: untyped) =
  ## Best-of-`reps`. Best rather than mean: the distribution is one-sided —
  ## preemption and page faults only ever make a run slower — so the minimum is
  ## the cleanest estimate of the code's own cost. The three `untyped` names
  ## are the caller's locals, so the template holds no state of its own.
  block:
    var best = high(int64)
    for r in 0 ..< reps:
      let t0 = getMonoTime().ticks
      body
      let dt = getMonoTime().ticks - t0
      if dt < best: best = dt
    digestOut = curDigest
    nsOut = best

proc wanted(name, only: string): bool =
  # `only in name` would be the idiomatic spelling, but Nimony has no substring
  # `contains` for strings and resolves `in` to the openArray overloads instead.
  # `find` is unambiguous in both compilers.
  only.len == 0 or find(name, only) >= 0

proc benchResults(calls, reps: int; only: string): seq[Result] =
  result = @[]
  var curDigest = 0'i64
  var ns = 0'i64
  var dig = 0'i64

  if wanted("reg3", only):
    timed(reps, curDigest, dig, ns):
      curDigest = runReg3(calls)
    result.add Result(name: "reg3", ns: ns, calls: calls, digest: dig)

  if wanted("pas1", only):
    timed(reps, curDigest, dig, ns):
      curDigest = runPas1(calls)
    result.add Result(name: "pas1", ns: ns, calls: calls, digest: dig)

  if wanted("pas2", only):
    timed(reps, curDigest, dig, ns):
      curDigest = runPas2(calls)
    result.add Result(name: "pas2", ns: ns, calls: calls, digest: dig)

  if wanted("pas2reg", only):
    timed(reps, curDigest, dig, ns):
      curDigest = runPas2Reg(calls)
    result.add Result(name: "pas2reg", ns: ns, calls: calls, digest: dig)

  if wanted("pas4", only):
    timed(reps, curDigest, dig, ns):
      curDigest = runPas4(calls)
    result.add Result(name: "pas4", ns: ns, calls: calls, digest: dig)

# ── formatting ──────────────────────────────────────────────────────────────

proc hundredths(v: int64): string =
  ## `v` is a count of hundredths; render it as `d.dd`. Integer-only, because
  ## `std/formatfloat` bottoms out in `snprintf` and a libc-free `nimony n`
  ## image has no libc to link it from — and because a benchmark whose numbers
  ## are rounded by three different runtimes is a benchmark you cannot diff.
  let whole = v div 100
  let frac = v mod 100
  result = $whole & "." & (if frac < 10: "0" else: "") & $frac

proc signedHundredths(v: int64): string =
  ## `hundredths` with an explicit sign, for a delta column.
  let neg = v < 0
  let u = if neg: 0'i64 - v else: v
  result = (if neg: "-" else: "+") & hundredths(u)

proc ratioStr(before, after: int64): string =
  ## Percentage change as `+d.d%`, in tenths so no float formatting is needed.
  if before <= 0: return "-"
  let diff = after - before
  let pct10 = (diff * 1000) div before
  let neg = pct10 < 0
  let u = if neg: 0'i64 - pct10 else: pct10
  result = (if neg: "-" else: "+") & $(u div 10) & "." & $(u mod 10) & "%"

proc fmtPerCall(ns: int64; calls: int): string =
  # Hundredths of a nanosecond per call: ns / calls * 100. `ns * 100` stays
  # well inside int64 at the largest `--calls`.
  if calls <= 0 or ns <= 0: return "0.00 ns"
  result = hundredths((ns * 100) div int64(calls)) & " ns"

# ── output ──────────────────────────────────────────────────────────────────

proc printResults(results: seq[Result]; csv: bool; total: int64) =
  when smoke:
    # The per-kernel digests alone carry the smoke verdict: they are stable, and
    # a table of durations is not something a golden can be diffed against.
    discard csv
    echo "| kernel | checksum |"
    echo "|:-------|---------:|"
    for r in results:
      echo "| ", r.name, " | ", r.digest, " |"
    echo "| **total** | **", total, "** |"
  else:
    if csv:
      echo "name,ns,calls,ns_per_call,checksum"
      for r in results:
        echo r.name, ",", r.ns, ",", r.calls, ",",
             (if r.calls > 0: $((r.ns * 100) div int64(r.calls)) else: "0"), ",",
             r.digest
    else:
      echo "| kernel | time | per call | checksum |"
      echo "|:-------|-----:|---------:|---------:|"
      for r in results:
        echo "| ", r.name, " | ", hundredths(r.ns div 10_000), " ms | ",
             fmtPerCall(r.ns, r.calls), " | ", r.digest, " |"
      echo "| **total** | | | **", total, "** |"

const HelpText = "passive_bench — time `.passive` call legs, or compare two toolchains\n" &
  "\n" &
  "  passive_bench [--calls:N] [--only:S] [--reps:N] [--csv] [--list]\n" &
  "\n" &
  "  --compare          build this source with two toolchains and diff them\n" &
  "  --feature:DIR      feature toolchain bin dir (default bin)\n" &
  "  --baseline:DIR     pre-feature toolchain bin dir (default ../nimony-master/bin)\n" &
  "  --source:PATH      source to build in compare mode (default bench/passive_bench.nim)\n" &
  "  --cpu:N            pin each run with taskset if available\n" &
  "  --runs:N           whole-program runs per toolchain (default " & $DefaultRuns & ")\n" &
  "  --calls:N          calls per kernel (default " & $DefaultCalls & ")\n" &
  "  --reps:N           repetitions inside a run, best kept (default " & $DefaultReps & ")\n" &
  "  --only:SUBSTR      run only kernels whose name contains SUBSTR\n" &
  "  --csv              emit machine-readable csv instead of a table\n" &
  "  --list             print the kernel names and exit"

proc toIntOpt(s: string; fallback: int): int =
  ## `parseInt` raises, and in Nimony that is viral: leaving it uncaught would
  ## push `.raises` onto every caller. Caught at the leaf.
  try: result = parseInt(s)
  except: result = fallback

# ── cross-toolchain comparison ──────────────────────────────────────────────

proc kernelIndex(name: string): int =
  for i in 0 ..< KernelNames.len:
    if KernelNames[i] == name: return i
  result = -1

proc makeDir(d: string): bool =
  try:
    createDir(path(d))
    result = true
  except:
    result = false

proc rmFile(p: Path) =
  try: removeFile(p)
  except: discard

proc rmDir(p: Path) =
  try: removeDir(p)
  except: discard

proc removeTree(d: string) =
  ## Best-effort recursive removal. `dirs.removeDir` removes a single (empty)
  ## directory, and a nimcache is neither empty nor a single level, so walk it
  ## and take the entries out first. `rmFile`/`rmDir` swallow their own errors
  ## so one stubborn entry does not abandon the rest of the tree.
  try:
    let dp = path(d)
    if dirExists(d):
      for (kind, p) in walkDir(dp):
        case kind
        of pcDir: removeTree($p)
        of pcFile, pcLinkToFile: rmFile(p)
        of pcLinkToDir: rmDir(p)
      rmDir(dp)
    elif fileExists(d):
      rmFile(dp)
  except:
    discard

proc runCapture(cmd: string; args: seq[string]; log: var string): int =
  ## Run `cmd` with `args` without a shell, capturing combined stdout and stderr
  ## into `log`. Returns the exit code, or -1 when the process could not start.
  result = -1
  log = ""
  try:
    let p = startProcess(cmd, args = args,
                         options = {poStdErrToStdOut, poUsePath})
    let (lines, code) = readLines(p)
    close(p)
    for l in lines:
      log.add l
      log.add '\n'
    result = code
  except:
    result = -1

proc buildTool(toolBin, src, cache, exe: string; log: var string): bool =
  ## Compile `src` with `toolBin/nimony`, pinning the binary at `exe` so no
  ## directory search is needed to find it afterwards.
  if not makeDir(cache):
    log = "cannot create cache directory: " & cache
    return false
  let code = runCapture(joinPath(toolBin, "nimony"),
    @["c", "--silentMake", "--opt:speed", "-d:danger",
      "--nimcache:" & cache, "-o:" & exe, src], log)
  result = code == 0 and fileExists(exe)

proc parseMin(raw: string): seq[int64] =
  ## The benchmark's csv is `name,ns,calls,ns_per_call,checksum`, with
  ## `ns_per_call` in hundredths of a nanosecond. Keep the minimum per kernel.
  result = @[]
  for i in 0 ..< KernelNames.len: result.add high(int64)
  for line in splitLines(raw):
    let f = split(line, ',')
    if f.len >= 4:
      let idx = kernelIndex(f[0])
      if idx >= 0:
        let v = int64(toIntOpt(f[3], -1))
        if v > 0 and v < result[idx]: result[idx] = v

proc runBench(runs: int; exe, cpu: string; calls, reps: int;
              log: var string): string =
  ## Run `exe` `runs` times, concatenating the csv it prints. `taskset` is used
  ## when a cpu is named and is available; a failure falls back to a free run.
  result = ""
  for ri in 0 ..< runs:
    var chunk = ""
    var code = -1
    if cpu.len > 0:
      code = runCapture("taskset",
        @["-c", cpu, exe, "--reps:" & $reps, "--calls:" & $calls, "--csv"], chunk)
    if code != 0:
      code = runCapture(exe,
        @["--reps:" & $reps, "--calls:" & $calls, "--csv"], chunk)
    if code == 0:
      result.add chunk
    else:
      log.add "benchmark run failed:\n" & chunk

proc compareToolchains(featureBin, baselineBin, src, cpu: string;
                       calls, reps, runs: int): string =
  ## Build once per toolchain, run each, and fold the two results into one
  ## markdown table. The control `reg3` must read the same on both sides; if it
  ## does not, the difference is codegen drift or frequency noise, not the
  ## feature.
  let tag = $getMonoTime().ticks
  let baseCache = joinPath(getTempDir(), "passive_bench_base_" & tag)
  let featCache = joinPath(getTempDir(), "passive_bench_feat_" & tag)
  let baseExe = joinPath(baseCache, "passive_bench")
  let featExe = joinPath(featCache, "passive_bench")

  var log = ""
  if not buildTool(baselineBin, src, baseCache, baseExe, log):
    return "**baseline build failed**\n\n```\n" & log & "```\n"
  if not buildTool(featureBin, src, featCache, featExe, log):
    removeTree(baseCache)
    return "**feature build failed**\n\n```\n" & log & "```\n"

  var runLog = ""
  let base = parseMin(runBench(runs, baseExe, cpu, calls, reps, runLog))
  let feat = parseMin(runBench(runs, featExe, cpu, calls, reps, runLog))

  removeTree(baseCache)
  removeTree(featCache)

  result = "feature/contextvars vs baseline, ns per call, min of " & $runs &
           " run(s) x best-of-" & $reps & "\n\n"
  result.add "| kernel | before (ns) | after (ns) | delta (ns) | ratio |\n"
  result.add "|:-------|------------:|-----------:|-----------:|------:|\n"
  for i in 0 ..< KernelNames.len:
    if base[i] == high(int64) or feat[i] == high(int64):
      continue
    result.add "| " & KernelNames[i] & " | " & hundredths(base[i]) & " | " &
               hundredths(feat[i]) & " | " & signedHundredths(feat[i] - base[i]) &
               " | " & ratioStr(base[i], feat[i]) & " |\n"
  if runLog.len > 0:
    result.add "\n```\n" & runLog & "```\n"

# ── main ────────────────────────────────────────────────────────────────────

proc main =
  var only = ""
  var reps = DefaultReps
  var calls = DefaultCalls
  var runs = DefaultRuns
  var csv = false
  var doCompare = false
  var featureBin = "bin"
  var baselineBin = joinPath(joinPath("..", "nimony-master"), "bin")
  var source = joinPath("bench", "passive_bench.nim")
  var cpu = ""

  for kind, key, val in getopt():
    case kind
    of cmdArgument: quit "unexpected argument: " & key
    of cmdLongOption, cmdShortOption:
      case key
      of "calls": calls = max(1, toIntOpt(val, DefaultCalls))
      of "only": only = val
      of "reps": reps = max(1, toIntOpt(val, DefaultReps))
      of "runs": runs = max(1, toIntOpt(val, DefaultRuns))
      of "compare": doCompare = true
      of "feature": featureBin = val
      of "baseline": baselineBin = val
      of "source", "src": source = val
      of "cpu": cpu = val
      of "csv": csv = true
      of "list":
        for n in KernelNames: echo n
        return
      of "version", "v":
        echo "passive_bench ", Version
        return
      of "help", "h":
        echo HelpText
        return
      else: quit "unknown option: " & key
    of cmdEnd: discard

  if doCompare:
    echo compareToolchains(featureBin, baselineBin, source, cpu, calls, reps, runs)
    return

  echo "passive_bench: ", calls, " calls per kernel, reps=", reps
  let results = benchResults(calls, reps, only)
  var total = 0'i64
  for r in results: total = total + r.digest
  printResults(results, csv, total)

main()
