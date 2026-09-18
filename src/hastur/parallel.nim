## The parallel test pool: a flat queue of work items, each run in its own
## `hastur test`/`hastur joined` subprocess with a private `nimcache/`, plus the
## shared-cache warmup and prebuild that keep a cold pool from thrashing.

import std / [syncio, os, osproc, strutils, times, typedthreads, locks, tables,
             algorithm]

import context, counters, category, concurrent

type WorkItem* = object
  ## One unit of work for the parallel pool: a single test file, a whole
  ## directory's joined group, or a directory owned by a `setup.nim` runner.
  ## `weight` is how many tests the unit accounts for, so the run's totals stay
  ## per-test even though a group is a single process.
  path*: string
  joined*: bool
  setup*: bool
    ## A `setup.nim` suite (`boot`, `validator`, …). Run by `hastur test <dir>`
    ## like any other item, so the suites overlap with the test pool instead of
    ## running one after the other before it starts.
  weight*: int
  native*: bool
    ## Compiled by `nimony n` (see `nativelist.walkUsesNative`). The worker
    ## decides this for itself from the same inputs; the parent needs to know
    ## it too, because the two backends take their cache prefill from
    ## different warmups and handing a native item the C one breaks its build.
  noPrefill*: bool
    ## Start from an empty cache rather than the warmup's. A `setup.nim` suite
    ## has no use for it, and `--noSystem`/`--compat` compiles must not see a
    ## `system` built without those flags: nifmake's staleness check is by
    ## mtime only, so it would take the prefilled one as up to date.


proc canRunParallel*(cat: Category): bool {.inline.} =
  ## Every category runs in the pool: each item gets a cache dir of its own,
  ## so the `nimcache/` reset that `Compat` and `Basics` need between tests is
  ## a given there. Only a serial (`--jobs:1`) run still shares one cache.
  true

proc prefillable*(cat: Category): bool {.inline.} =
  ## See `WorkItem.noPrefill`.
  cat notin {Compat, Basics}

# ---- scheduling: longest first, by what the previous runs measured ----------

const TimesFile = "nimcache_static" / "hastur.times"
  ## How long each work item took the last time it ran, one `seconds<TAB>path`
  ## per line. `nimcache_static/` because `nimcache/` itself is wiped by every
  ## `hastur build`, i.e. by every tree walk.

proc itemKey(it: WorkItem): string = normalizeDirKey(it.path)

proc loadTimes(): Table[string, float] =
  result = initTable[string, float]()
  if not fileExists(TimesFile): return
  try:
    for line in lines(TimesFile):
      let tab = line.find('\t')
      if tab > 0:
        try: result[line.substr(tab + 1)] = parseFloat(line.substr(0, tab - 1))
        except ValueError: discard
  except IOError, OSError:
    discard

proc saveTimes(times: Table[string, float]) =
  var keys: seq[string] = @[]
  for k in keys(times): keys.add k
  sort keys
  var s = ""
  for k in keys:
    s.add formatFloat(times[k], ffDecimal, precision = 2)
    s.add '\t'
    s.add k
    s.add '\n'
  try:
    createDir TimesFile.parentDir
    writeFile(TimesFile, s)
  except IOError, OSError:
    discard  # only an ordering hint; the next run just guesses again

proc scheduleLongestFirst*(items: var seq[WorkItem]) =
  ## Put the long poles first so the pool's tail is short. An item that has
  ## run before is ranked by its measured duration; a new one is guessed from
  ## its weight, and a `setup.nim` suite, which is typically a whole build (or
  ## three, for `boot`), goes to the front.
  let times = loadTimes()
  proc estimate(it: WorkItem): float =
    result = times.getOrDefault(itemKey(it), -1.0)
    if result < 0:
      result = if it.setup: 1e9 else: it.weight.float * 5.0
  sort items, proc (a, b: WorkItem): int =
    result = cmp(estimate(b), estimate(a))
    if result == 0: result = cmp(a.path, b.path)

proc warmupSharedCache(native = false): string =
  ## Compile `tools/warmup.nim` once into `nimcache/warmup/` so each
  ## parallel test can start with system + common stdlib bundles already
  ## present. Returns the warmup cache directory, or "" on opt-out
  ## (warmup source missing or compile failed — tests still work, just
  ## without the savings).
  ##
  ## `native` seeds the OTHER pipeline's cache, in a directory of its own.
  ## The two must not mix: the intermediates the C and native backends read
  ## share file names but not content — a native compile handed the C run's
  ## bundle of `system` gets one whose externs have lost their `dynlib`, and
  ## arkham stops at the first of them ("`GetStdHandle` names no import
  ## library"). So a native work item prefills from here and a C one from
  ## there, and neither ever sees the other's files.
  const warmupSrc = "tools/warmup.nim"
  if not fileExists(warmupSrc):
    # Loud, because the fallback is silent-but-slow: without the prefill every
    # test recompiles `system` from scratch (~3s each on Windows CI, ~700
    # tests). A missing warmup file previously just disabled the optimization
    # with no output at all, so the regression was invisible.
    stderr.writeLine "warmup: " & warmupSrc &
      " missing; every test will recompile the stdlib from scratch"
    return ""
  result = nimcacheDir / (if native: "warmup_native" else: "warmup")
  let nimony = toolExe("nimony")
  if not fileExists(nimony):
    stderr.writeLine "warmup: skipping, no nimony at " & nimony
    return ""
  # The same command the work items are compiled with, so what lands here is
  # exactly what they would have produced themselves (`execNimonyNative` for
  # the native side, `execNimony`'s `c` for the other).
  let cmd = nimony.quoteShell &
            (if native: " n --silentMake --isMain --nimcache:" else: " c --nimcache:") &
            result.quoteShell & " " & warmupSrc.quoteShell
  let t0 = epochTime()
  let exit = execShellCmd(cmd)
  let dt = epochTime() - t0
  if exit != 0:
    stderr.writeLine "warmup: skipping (compile failed): " & cmd
    return ""
  if dt > 0.5:
    # Only report cold compiles; subsequent calls in the same `hastur all`
    # run are sub-second no-ops thanks to nimony's incremental build, and
    # printing them per-category just adds noise.
    echo "warmup compiled in ", formatFloat(dt, ffDecimal, precision=2), "s."

var sharedObjectsPrebuilt = false
  ## `nimcache_static/` holds object files that don't depend on per-project
  ## state and are reused across every build — currently just mimalloc's
  ## `static.o`. nifmake's `needsRebuild` is purely mtime-based, so when that
  ## `.o` is missing on a cold cache every parallel worker independently
  ## decides to (re)compile `static.c` into the same shared path at once.
  ## The concurrent `cc … -o static.o` writes clobber each other and a
  ## half-written object links with `undefined reference to mi_malloc`. We
  ## build it once, serially, before any worker starts; thereafter the file
  ## exists and every worker's staleness check skips it.

proc prebuildSharedObjects(forward: string) =
  ## Compile a trivial program once so the shared `nimcache_static/` object
  ## files exist before the parallel pool launches. Idempotent across the
  ## many `parallelTestDir` calls in a single `hastur` run (only the first
  ## does real work; once `static.o` is present the build is a no-op).
  ##
  ## `forward` MUST be the same flag string the test workers pass to nimony
  ## (e.g. `--cc:clang` on Windows CI). `static.o` lands in the shared
  ## `nimcache_static/` and is keyed only by mtime, so once we build it the
  ## workers reuse it verbatim — if we built it with a different compiler than
  ## the workers link with, the result is an ABI mismatch. Concretely: on
  ## Windows the tester forwards `--cc:clang` (clang uses native PE TLS); a
  ## prebuild with the default gcc emits gthr/emulated-TLS `static.o`, and the
  ## clang+lld worker link then fails with `undefined symbol: pthread_*`.
  if sharedObjectsPrebuilt: return
  sharedObjectsPrebuilt = true
  let nimony = toolExe("nimony")
  if not fileExists(nimony):
    return
  let cache = nimcacheDir / "prebuild_static"
  let src = cache / "prebuild_static.nim"
  try:
    createDir cache
    # Any program that pulls in `system` triggers the mimalloc `static.c`
    # build pragma; a bare `discard` is enough.
    writeFile(src, "discard\n")
  except OSError, IOError:
    return
  var cmd = nimony.quoteShell & " c --silentMake --nimcache:" & cache.quoteShell
  # Same compiler/link flags the workers use (`--cc:`, `--passL:` …), so the
  # shared `static.o` matches the toolchain the workers link with.
  if forward.len > 0:
    cmd.add ' '
    cmd.add forward
  # Match `testFile`'s per-platform flags so the prebuilt `static.o` is the
  # exact artifact the tests want (valgrind-tracked mimalloc on Linux).
  #
  # mimalloc's build pragma no longer bakes in `-DMI_TRACK_VALGRIND=1` (that
  # made the valgrind dev headers a hard build dependency for every nimony
  # program); valgrind tracking is now requested purely via this `--passC`.
  # But the shared `static.o` is keyed only by mtime, so a prior *non*-valgrind
  # build (e.g. a plain `bin/nimony c foo.nim`) can leave a stale, untracked
  # `static.o` that nifmake would happily reuse — silently running the valgrind
  # tests against non-tracked mimalloc. Delete it so this valgrind-tracked
  # variant is always freshly produced.
  when defined(linux):
    if hasValgrind:
      try: removeFile("nimcache_static" / "static.o")
      except OSError: discard
      # The valgrind tests compile with `-d:useLibc` (valgrind can only track the
      # libc/mimalloc heap; the native mmap heap has no hooks), so the shared
      # `static.o` they reuse must be the *mimalloc* object — build the prebuild
      # probe with `-d:useLibc` too. Without it the libc-free default is used and
      # `static.o` is never produced (mimalloc isn't compiled), so valgrind runs
      # against an untracked heap and reports 0 allocations.
      cmd.add " -d:useLibc --passC:\"-DMI_TRACK_VALGRIND=1\""
  cmd.add ' ' & src.quoteShell
  if execShellCmd(cmd) != 0:
    # Non-fatal: if this fails the tests still run, just without the
    # pre-built shared object (and may hit the original race). Surface it so
    # the cause is visible rather than silently degrading.
    stderr.writeLine "prebuild: shared object compile failed: " & cmd

proc copyPreservingMtime(src, dst: string) =
  ## Copy `src` to `dst` and stamp `dst` with `src`'s mtime. Mtime
  ## preservation is load-bearing: `nifmake.needsRebuild` keys off
  ## output-mtime > input-mtime ordering, so a fresh "now" mtime on every
  ## prefilled file would scramble the DAG-order mtimes the warmup set
  ## up and trigger spurious recompiles. Hardlinks would also preserve
  ## mtimes "for free", but they share an inode — when one parallel test
  ## triggers an in-place rewrite of a shared bundle (say a config
  ## difference forces a recompile), every other test holding a hardlink
  ## sees the truncated/partial content and crashes. Copying gives each
  ## test an independent inode, paid for once at prefill.
  try:
    copyFile(src, dst)
    try: setLastModificationTime(dst, getLastModificationTime(src))
    except: discard
  except OSError, IOError:
    discard  # best-effort; falling back to a cold per-test compile is fine

proc prefillFromWarmup*(warmupCache, cacheDir: string) =
  ## Called by the WORKER (`--prefill:`), not by the pool: a prefill is ~130
  ## file copies, which on Windows costs a third of a second, and done by the
  ## parent it was ~100s of a run spent on the one thread that launches the
  ## next item — with the slots waiting for it.
  if warmupCache.len == 0 or not dirExists(warmupCache):
    return
  for path in walkDirRec(warmupCache, yieldFilter = {pcFile}, relative = true):
    let dst = cacheDir / path
    try: createDir(dst.parentDir)
    except OSError: discard
    copyPreservingMtime(warmupCache / path, dst)

proc parallelTestDir*(c: var TestCounters; items: openArray[WorkItem];
                     overwrite: bool; cat: Category; forward: string;
                     jobs: int) =
  ## Run each work item in its own subprocess (`bin/hastur test ...` for a
  ## file or a `setup.nim` directory, `bin/hastur joined ...` for a
  ## directory's group) with a per-item `--cacheDir` so concurrent
  ## compilations cannot collide on intermediates. Up to `jobs` subprocesses
  ## run at once, in the order of `items`. Test results are streamed in
  ## completion order; final pass/fail counts go into the shared `c`.
  let hastur = getAppFilename()
  prebuildSharedObjects(forward)
  var anyPrefill = false
  var anyNative = false
  for it in items:
    if not it.noPrefill:
      anyPrefill = true
      if it.native: anyNative = true
  let warmupCache = if anyPrefill: warmupSharedCache() else: ""
  # Seeded only when something in this run actually wants it: on every host but
  # Windows no item is native, and paying for a second warmup compile there
  # would be pure loss.
  let nativeWarmupCache = if anyNative: warmupSharedCache(native = true) else: ""
  let parallelStart = epochTime()
  var times = loadTimes()
  let queue = @items   # `launch` captures it; an openArray cannot be
  var head = 0

  type Slot = object
    p: Process
    idx: int
    item: WorkItem
    started: float
    reader: Thread[ReaderArg]
  var slots = newSeq[Slot](jobs)
  var active = 0
  var stdoutLock = default(Lock)
  initLock(stdoutLock)

  proc launch(slot: int) =
    if head >= queue.len: return
    let idx = head
    let item = queue[head]
    inc head
    let cacheDir = nimcacheDir / ".par" / $idx
    var args = @[(if item.joined: "joined" else: "test"),
                 "--no-build", "--cachedir:" & cacheDir, "--scratch"]
    if not item.noPrefill:
      let warmup = if item.native: nativeWarmupCache else: warmupCache
      if warmup.len > 0: args.add "--prefill:" & warmup
    # Forward the parent's resolved toolchain dir so each worker uses the
    # exact same binaries (the default is now hastur's own sibling dir, an
    # absolute path, not the literal "bin").
    args.add "--bindir:" & toolchainDir
    # A work item's backend is settled HERE, by the parent, because the parent
    # is what prefills the cache — and the two warmups cannot be mixed (see
    # `warmupSharedCache`). The worker would reach the same verdict on its own
    # for the item as planned, but not on every path through it: a joined group
    # that diverges re-runs its members one by one, and in a group that is not
    # all-native some of those members are individually eligible. That re-run
    # inherits the C prefill, so it must inherit the C backend with it.
    when defined(windows):
      if not item.native: args.add "--native:off"
    if overwrite: args.add "--overwrite"
    if forward.len > 0: args.add "--forward:" & forward
    args.add item.path
    let p = startProcess(hastur, args = args,
        options = {poStdErrToStdOut, poUsePath})
    slots[slot] = Slot(idx: idx, item: item, p: p, started: epochTime())
    let arg = ReaderArg(handle: p.outputHandle.int,
                        lockPtr: cast[pointer](addr stdoutLock))
    createThread(slots[slot].reader, drainStdout, arg)
    inc active

  for s in 0 ..< jobs: launch(s)

  while active > 0:
    for s in 0 ..< jobs:
      if slots[s].p == nil: continue
      let exit = peekExitCode(slots[s].p)
      if exit != -1:
        # Child exited. Worker's read loop hits EOF, flushes its
        # accumulated buffer to stdout under the lock, and exits.
        # `joinThread` waits for that flush to complete before we
        # tally the result and reuse the slot.
        joinThread(slots[s].reader)
        slots[s].p.close()
        times[itemKey(slots[s].item)] = epochTime() - slots[s].started
        inc c.total, slots[s].item.weight
        if exit != 0:
          # A `test` worker exits 1; a `joined` worker exits with how many of
          # its members failed, so a group cannot hide a second failure.
          let failed = min(exit, slots[s].item.weight)
          inc c.failures, failed
          # The worker already printed which of its members failed; up here
          # only the unit has a name, so that plus the count is what the
          # summary can replay.
          c.failed.add(if failed > 1:
                         slots[s].item.path & " (" & $failed & " tests)"
                       else: slots[s].item.path)
        slots[s].p = nil
        dec active
        launch(s)
    if active > 0:
      sleep(2)

  deinitLock(stdoutLock)
  saveTimes(times)
  echo "parallel run: ",
       formatFloat(epochTime() - parallelStart, ffDecimal, precision=2), "s."
