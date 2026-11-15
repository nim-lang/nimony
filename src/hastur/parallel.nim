## The parallel test pool: a flat queue of work items, each run in its own
## `hastur test`/`hastur joined` subprocess with a private `nimcache/`, plus the
## shared-object prebuild that keeps a cold pool from thrashing. The stdlib
## needs no such help: every item's compile shares one stdlib cache, which
## nifmake builds once under its lock (`deps.stdCacheDir`).

import std / [syncio, os, osproc, strutils, times, typedthreads, locks, tables,
             algorithm]

import context, counters, category, concurrent
from nativelist import walkNative

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


proc canRunParallel*(cat: Category): bool {.inline.} =
  ## Every category runs in the pool: each item gets a cache dir of its own,
  ## so the `nimcache/` reset that `Compat` and `Basics` need between tests is
  ## a given there. Only a serial (`--jobs:1`) run still shares one cache.
  true

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
  ## (e.g. `--cc:clang` on Windows CI). The shared object's name is keyed by
  ## its whole compiler command line, so a prebuild with other flags would not
  ## be wrong — it would just build an object the workers never use, and they
  ## would race on building theirs.
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
  var cmd = nimony.quoteShell & " c --silentMake " & parallelBuildFlag() &
            "--nimcache:" & cache.quoteShell
  # Same compiler/link flags the workers use (`--cc:`, `--passL:` …), so the
  # shared `static.o` matches the toolchain the workers link with.
  if forward.len > 0:
    cmd.add ' '
    cmd.add forward
  # Match `testFile`'s flags (`nimonyCmdFor`) so the prebuilt `static.o` is the
  # exact artifact the tests want. Only a `-d:useLibc` build compiles mimalloc
  # at all — the libc-free default uses the native allocator — and the tests
  # that link `static.o` are exactly the ones `nimonyCmdFor` gives `-d:useLibc`
  # (the valgrind category, `.valgrind` and golden `.nim.c` tests). They do so
  # whether valgrind is installed or not, so the probe must too: without it
  # nothing is prebuilt, and on a valgrind-less box (every CI runner) those
  # tests all compile `static.c` into the shared path at once.
  cmd.add " -d:useLibc"
  when defined(linux):
    if hasValgrind:
      # mimalloc's build pragma no longer bakes in `-DMI_TRACK_VALGRIND=1`
      # (that made the valgrind dev headers a hard build dependency for every
      # nimony program); valgrind tracking is requested purely via this
      # `--passC`. But the shared `static.o` is keyed only by mtime, so a prior
      # *non*-valgrind build (e.g. a plain `bin/nimony c foo.nim`) can leave a
      # stale, untracked `static.o` that nifmake would happily reuse — silently
      # running the valgrind tests against non-tracked mimalloc. Delete it so
      # this valgrind-tracked variant is always freshly produced.
      try: removeFile(getCacheDir("nimony") / "nimcache_static" / "static.o")
      except OSError: discard
      cmd.add " -d:useLibc --passC:\"-DMI_TRACK_VALGRIND=1\""
  cmd.add ' ' & src.quoteShell
  if execShellCmd(cmd) != 0:
    # Non-fatal: if this fails the tests still run, just without the
    # pre-built shared object (and may hit the original race). Surface it so
    # the cause is visible rather than silently degrading.
    stderr.writeLine "prebuild: shared object compile failed: " & cmd

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
    # Forward the parent's resolved toolchain dir so each worker uses the
    # exact same binaries (the default is now hastur's own sibling dir, an
    # absolute path, not the literal "bin").
    args.add "--bindir:" & toolchainDir
    if parallelBuild > 0: args.add "--parallelBuild:" & $parallelBuild
    # each worker decides for itself which tests compile with `nimony n`
    # (`nativelist.walkUsesNative`), unless the run said not to
    if not walkNative: args.add "--native:off"
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
