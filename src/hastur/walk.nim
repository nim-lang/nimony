## The recursive tree runner behind `hastur <dir>` and `hastur all`, and how
## every parallel-safe test in the tree ends up in one saturated pool.
##
## A directory describes how it is tested with two optional files:
##   setup.nim     — a custom runner program that OWNS the directory (and its
##                   subtree): hastur compiles+runs it, passes context on argv,
##                   and takes its exit code as the verdict. This is the escape
##                   hatch for suites that aren't "a folder of inputs" (boot,
##                   incremental, validator) or need bespoke logic (dagon,
##                   pnak). It imports `kit.nim` as the test kit.
##   setup.hastur  — lightweight prep for a directory still run by the built-in
##                   nimony runner: each line is a hastur subcommand (e.g.
##                   `build nimony`), run before the tests below it.
## Neither present → the built-in nimony runner (`runner.nim`) processes the
## directory's own `.nim` files (category from its `hastur.mode`) and recursion
## continues.

import std / [syncio, os, osproc, strutils, times, algorithm]
import context, counters, category, joined, nativelist, parallel, runner

proc runSetupHastur*(dir: string) =
  ## Prep step for a built-in-runner directory: run each line of
  ## `<dir>/setup.hastur` as a hastur subcommand before its tests. `--debug`
  ## is forwarded so a `--debug` run also builds the toolchain unoptimized
  ## (the child hastur wouldn't otherwise inherit it); without it the child
  ## builds release, same as the parent's default.
  if skipBuild: return
  let f = dir / "setup.hastur"
  if not fileExists(f): return
  let self = getAppFilename().quoteShell
  let relFlag = if debugBuild: " --debug" else: ""
  for raw in lines(f):
    let line = raw.strip
    if line.len == 0 or line.startsWith("#"): continue
    exec self & relFlag & " " & line, showProgress = true

proc runSetupNimDir*(c: var TestCounters; dir, forward: string; overwrite: bool) =
  ## A `setup.nim` owns its directory. Compile and run it, passing context on
  ## argv (the test dir, toolchain dir, cache dir, overwrite, forwarded
  ## flags); its exit code is the directory's verdict. The program reports its
  ## own per-test detail, so here the whole directory counts as one result.
  inc c.total
  let setupNim = dir / "setup.nim"
  let cache = nimcacheDir / "setupnim" / dir.splitPath.tail
  # `-o:` keeps the compiled runner in the cache; without it nim drops the
  # binary next to `setup.nim` and litters the test tree.
  let outBin = cache / "setup".addFileExt(ExeExt)
  var cmd = "nim c -r --warningAsError:ProveInit:off --warningAsError:Uninit:off" &
            " --nimcache:" & cache.quoteShell & " -o:" & outBin.quoteShell & " " &
            setupNim.quoteShell & " --" &
            " --dir:" & dir & " --bindir:" & toolchainDir & " --cachedir:" & nimcacheDir
  if overwrite: cmd.add " --overwrite"
  if forward.len > 0: cmd.add " --forward:" & forward
  if execShellCmd(cmd) != 0:
    # The runner printed its own per-test detail; name the suite so the
    # top-level summary still points somewhere.
    noteFailure c, dir & " (setup.nim runner)"

type WalkPlan = object
  ## Accumulated during the tree walk so the run phase can drive ONE
  ## saturated parallel pool instead of one pool per directory. The old
  ## walk ran each leaf directory to completion before starting the next,
  ## so every directory boundary was a hard pool barrier (N-1 cores idle on
  ## its tail test) plus a fresh `warmupSharedCache` spawn. Flattening every
  ## parallel-safe file into a single queue pays warmup/prebuild once and
  ## keeps the pool full across the whole run — the win is largest where
  ## process spawns are expensive (Windows CI).
  parItems: seq[WorkItem]            # parallel-safe units of work, flattened
  serialDirs: seq[(string, Category)] # dirs that must run serially (see below)

proc collectTests*(c: var TestCounters; plan: var WalkPlan; dir, forward: string;
                  overwrite, isRoot: bool) =
  # `--skip:` is honoured even for an explicit root: it says "not in this run",
  # so a caller that names both is asking for nothing rather than for a fight.
  if normalizeDirKey(dir) in skipDirs: return
  # `hastur.mode = skip` excludes a directory from the sweep, but only when the
  # walk *descends* into it — pointing hastur straight at it (isRoot) still
  # runs it. That's how a WIP/known-broken suite (e.g. dagon) stays out of the
  # default `all` run yet remains explicitly runnable via `hastur tests/dagon`.
  let cat = categoryOfDir(dir)
  if cat == Skip and not isRoot: return
  if fileExists(dir / "setup.nim"):
    # A `setup.nim` owns its subtree and runs its own tests right here — it is
    # a self-contained runner, not part of the shared file pool.
    runSetupNimDir(c, dir, forward, overwrite)
    return
  # `setup.hastur` prep (e.g. building the toolchain) must precede every test
  # in its subtree, so it runs during the walk, before the run phase kicks off.
  runSetupHastur(dir)
  var hasNim = false
  var subs: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim"): hasNim = true
    elif x.kind == pcDir: subs.add x.path
  if hasNim:
    # A grouping directory has no `.nim` files of its own, so a stray one
    # dropped into `tests/` demotes the whole tree to a "leaf" and silently
    # skips every suite below it — the run still says SUCCESS, just for 7
    # tests instead of 672. Nothing distinguishes the two roles except this:
    # a real leaf's subdirectories are import fixtures (`deps/`, `imp/`, …)
    # and never carry a runner marker. If one does, the `.nim` here is the
    # mistake, so say which file and stop rather than quietly test nothing.
    for s in subs:
      if fileExists(s / "setup.nim") or fileExists(s / ModeFile):
        var strays: seq[string] = @[]
        for x in walkDir(dir):
          if x.kind == pcFile and x.path.endsWith(".nim"): strays.add x.path
        quit "FAILURE: " & dir & " groups test suites (" & s &
             " is one) but also holds test files:\n  " & strays.join("\n  ") &
             "\nMove them into a suite directory — left here they hide the whole tree."
    # Leaf test directory: gather its own `.nim` files and do NOT descend.
    # Nested dirs here (`deps/`, `imp/`, `system/`, …) hold import fixtures
    # pulled in by those tests, not standalone tests — the old per-category
    # runner never entered them either.
    if parallelJobs > 1 and canRunParallel(cat):
      # The directory's plain tests become ONE unit (a `joined` worker compiles
      # them into a single program); whatever cannot be joined stays a unit of
      # its own. Both kinds go into the same flat queue.
      let members = joinMembers(dir, cat, overwrite)
      let joined = members.len >= MinJoinGroup
      if joined:
        # Mirrors `joinedTest`'s own all-or-nothing rule, so the prefill the
        # parent hands the worker is the one the worker will want.
        var groupNative = nativeJoinable(dir)
        if groupNative:
          for f in members:
            if not walkUsesNative(f, cat): groupNative = false; break
        plan.parItems.add WorkItem(path: dir, joined: true, weight: members.len,
                                   native: groupNative)
      for x in walkDir(dir):
        if x.kind == pcFile and x.path.endsWith(".nim") and
           not isGeneratedTestFile(x.path) and
           not (joined and joinable(x.path, cat)):
          plan.parItems.add WorkItem(path: x.path, weight: 1,
                                     native: walkUsesNative(x.path, cat))
    else:
      # `Basics`/`Compat` reset the shared `nimcache/` around their loop and so
      # cannot share the pool's cache layout; serial (`--jobs:1`) runs keep the
      # in-process `testFile` path. Either way, defer to a per-dir `testDir`.
      plan.serialDirs.add (dir, cat)
  else:
    # Pure grouping directory (e.g. `tests/`, `tests/nimony/`): recurse.
    sort subs
    for s in subs: collectTests(c, plan, s, forward, overwrite, isRoot = false)

proc walkRoots*(roots: openArray[string]; forward: string; overwrite: bool) =
  ## Run one or more test trees, accumulating into shared counters and
  ## reporting once. `hastur <dir>` passes a single root; `all` passes
  ## `tests/` and `examples/`.
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  var plan = WalkPlan()
  for r in roots:
    if not dirExists(r): quit "FAILURE: not a directory: " & r
    collectTests(c, plan, r, forward, overwrite, isRoot = true)
  # Serial suites first: `Basics`/`Compat` wipe the whole `nimcache/`, which
  # would delete the pool's per-test cache dirs if it ran afterwards. They
  # finish and reset the cache, then the single flat pool populates it.
  for (d, cat) in plan.serialDirs:
    testDir(c, d, overwrite, cat, forward)
  if plan.parItems.len > 0:
    # Biggest units first: a joined group is many tests in one process, so
    # starting the long poles early keeps the pool's tail short.
    sort plan.parItems, proc (a, b: WorkItem): int =
      result = cmp(b.weight, a.weight)
      if result == 0: result = cmp(a.path, b.path)
    # One saturated pool over every parallel-safe unit from every directory.
    # `parallelTestDir` ignores the `cat` argument (each worker re-derives its
    # own category from its path's directory), so a mixed-category queue is safe.
    parallelTestDir(c, plan.parItems, overwrite, Normal, forward, parallelJobs)
  reportFailures c
  echo c.total - c.failures, " / ", c.total, " tests successful in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0: quit "FAILURE: Some tests failed."
  else: echo "SUCCESS."

proc walkCmd*(dir, forward: string; overwrite: bool) =
  ## The general entry point: `hastur <dir>` runs the whole test tree at
  ## `<dir>`. `hastur tests/` is what `all` becomes.
  walkRoots([dir], forward, overwrite)
