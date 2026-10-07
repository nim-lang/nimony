## The incremental-build regression: drive `nimony c --report` through a fixed
## sequence of scenarios and assert which phases actually re-ran.

import std / [syncio, os, osproc, strutils, times, algorithm, sequtils]

import context

# ---- Incremental-build regression test ------------------------------------
# `nifmake --report` prints a machine-readable summary of which commands
# actually executed during one nifmake invocation. We drive `bin/nimony c
# --report` over `tests/incremental/sample.nim` through a sequence of
# scenarios and assert on the per-phase counts. This catches mtime-tracking
# regressions (e.g. tools that drift back into "always rewrite" and trigger
# perpetual rebuilds, or staleness checks that miss real edits) without
# the brittleness of comparing file timestamps.

type ReportEntry = tuple[cmd: string, count: int]

proc parseNifmakeReports*(output: string): seq[seq[ReportEntry]] =
  ## Each `nifmake-report …` line in `output` becomes one inner seq. Lines
  ## without entries (an up-to-date no-op) yield an empty seq plus the
  ## sentinel `total=0` entry that nifmake always emits.
  result = @[]
  for line in output.splitLines:
    if not line.startsWith("nifmake-report"): continue
    var entries: seq[ReportEntry] = @[]
    for part in line.split(' '):
      if part.len == 0 or part == "nifmake-report": continue
      let eq = part.find('=')
      if eq < 0: continue
      try: entries.add((part[0 ..< eq], parseInt(part[eq+1 .. ^1])))
      except ValueError: discard
    result.add entries

proc reportField*(entries: seq[ReportEntry]; cmd: string): int =
  for e in entries:
    if e.cmd == cmd: return e.count
  result = 0

proc filesBySuffix(cache, suffix: string): seq[string] =
  result = @[]
  for f in walkDirRec(cache):
    if f.endsWith(suffix): result.add f
  sort result

proc mainHexedPerBackend(cache: string): seq[(string, string)] =
  ## `(backend directory name, content of the main module's .x.nif)` for every
  ## backend that has built under `cache`. `deps.backendDirName` puts each
  ## backend's main-specific artifacts into `<tag>/<mainmod>/`, so this is one
  ## entry per backend that has built here. A plugin's sub-compile caches sit
  ## in `cache` too and have backend directories of their own: skip them.
  result = @[]
  for kind, tagDir in walkDir(cache):
    if kind != pcDir or isPluginScratchDir(cache, tagDir.lastPathPart): continue
    for kind2, mainDir in walkDir(tagDir):
      if kind2 != pcDir: continue
      for f in walkFiles(mainDir / "*.x.nif"):
        result.add (tagDir.lastPathPart, readFile(f))
  sort result

proc incrementalTests*() =
  ## Drive `bin/nimony c --report` through a fixed sequence of scenarios on
  ## `tests/incremental/sample.nim` and assert the per-nifmake-invocation
  ## command counts. Fails the run on the first divergence; restores the
  ## sample file regardless of outcome.
  let t0 = epochTime()
  let src = "tests/incremental/sample.nim"
  let dep = "tests/incremental/inlinedep.nim"
  let cache = "nimcache" / "incremental"
  let nimony = "bin" / "nimony".addFileExt(ExeExt)
  for f in [src, dep]:
    if not fileExists(f):
      quit "incremental: " & f & " missing"
  if not fileExists(nimony):
    quit "incremental: " & nimony & " not found; run `hastur build nimony` first"
  removeDir cache

  # `-r` so every phase also RUNS the result: a rebuild that nifmake skipped
  # when it should not have leaves a stale binary behind, and a report count
  # alone would not notice (see the `inline-dep` phase).
  let baseCmd = nimony.quoteShell & " c -r --silentMake --report --nimcache:" &
                cache.quoteShell & " " & src.quoteShell
  let originalSrc = readFile(src)
  let originalDep = readFile(dep)

  proc restoreSources() =
    writeFile(src, originalSrc)
    writeFile(dep, originalDep)

  var lastOutput = ""
  proc run(label: string; cmd = baseCmd): seq[seq[ReportEntry]] =
    let (output, ec) = execCmdEx(cmd)
    lastOutput = output
    if ec != 0:
      stdout.write output
      restoreSources()
      quit "incremental: '" & label & "' compile failed"
    parseNifmakeReports(output)

  var failures: seq[string] = @[]
  template expect(cond: bool; msg: string) =
    if not (cond): failures.add msg

  # Phase 1: cold cascade — both nifmake invocations should run real work.
  block:
    let r = run("cold")
    expect r.len == 2, "cold: expected 2 nifmake invocations, got " & $r.len
    if r.len == 2:
      expect reportField(r[0], "total") > 0, "cold: frontend ran 0 commands"
      expect reportField(r[1], "total") > 0, "cold: backend ran 0 commands"

  # Phase 2: no-op rebuild — both reports should show total=0.
  block:
    let r = run("noop")
    if r.len == 2:
      expect reportField(r[0], "total") == 0,
             "noop: frontend re-ran " & $reportField(r[0], "total") & " commands"
      expect reportField(r[1], "total") == 0,
             "noop: backend re-ran " & $reportField(r[1], "total") & " commands"

  # Phase 3: touch (no content change). Only interface files are written
  # unchanged-is-untouched (doc/internals/ic.md), so the touched module's own
  # chain runs once — sem, hexer, emit, codegen, C compiler, link — and its
  # `.s.idx.nif`/`.c.idx.nif` spare every other module. A second build has
  # nothing to do: a node that ran is up to date even when its output came
  # out the same.
  block:
    setLastModificationTime(src, getTime())
    var r = run("touch")
    if r.len == 2:
      expect reportField(r[0], "nimsem") == 1,
             "touch: nimsem ran " & $reportField(r[0], "nimsem") & " times (expected 1)"
      expect reportField(r[1], "lengc") == 1,
             "touch: lengc ran " & $reportField(r[1], "lengc") & " times (expected 1)"
      expect reportField(r[1], "cc") == 1,
             "touch: cc ran " & $reportField(r[1], "cc") & " times (expected 1)"
    r = run("touch-settle")
    if r.len == 2:
      expect reportField(r[0], "total") == 0 and reportField(r[1], "total") == 0,
             "touch-settle: re-ran " & $(reportField(r[0], "total") + reportField(r[1], "total")) &
             " commands (expected 0)"

  # Phase 4: real content edit — full cascade.
  block:
    writeFile(src, originalSrc & "\necho \"incremental edited\"\n")
    let r = run("edit")
    if r.len == 2:
      expect reportField(r[0], "nimsem") >= 1,
             "edit: nimsem did not re-run"
      expect reportField(r[1], "total") > 0,
             "edit: backend ran 0 commands"
    # Undo the edit and let the cache settle on the restored file, so the next
    # phase's only change is the one it makes itself.
    writeFile(src, originalSrc)
    discard run("resettle")

  # Phase 5: edit an IMPORTED module's `.inline` proc and nothing else. The
  # importer's own `.c.nif` still says only "call bump"; the body is spliced
  # in one stage later, by lengc, out of the callee's `.c.nif`. So the
  # importer's codegen depends on a file that is not its own input unless
  # `deps.addInlineSourceInputs` declares it, and without that edge nifmake
  # leaves the importer's `.c` untouched: a link error when the edit moves a
  # symbol the splice names, and a silently stale binary when it does not
  # (nim-lang/nimony#1897). Two `.c` files must be regenerated here — the
  # callee's, because its body changed, and the importer's, because of the
  # splice — and the program has to print the NEW value.
  block:
    writeFile(dep, originalDep.replace("x + 1", "x + 1000"))
    let r = run("inline-dep")
    if r.len == 2:
      expect reportField(r[1], "lengc") >= 2,
             "inline-dep: lengc ran " & $reportField(r[1], "lengc") &
             " times (expected the callee's and the importer's)"
    expect lastOutput.contains("1010"),
           "inline-dep: ran a stale inlined body; expected the program to print 1010"

  restoreSources()
  discard run("inline-dep-settle")

  # Phase 5b: edit the body of an imported proc that is NOT `.inline`. No
  # other module reads it — the importer's code only calls it — so the
  # callee's `.c.idx.nif` stays as it was and exactly one translation unit is
  # regenerated and compiled (doc/internals/ic.md).
  block:
    writeFile(dep, originalDep.replace("x * 3", "x * 5"))
    let r = run("body-dep")
    if r.len == 2:
      expect reportField(r[0], "nimsem") == 1,
             "body-dep: nimsem ran " & $reportField(r[0], "nimsem") & " times (expected 1)"
      expect reportField(r[1], "lengc") == 1,
             "body-dep: lengc ran " & $reportField(r[1], "lengc") & " times (expected 1)"
      expect reportField(r[1], "cc") == 1,
             "body-dep: cc ran " & $reportField(r[1], "cc") & " times (expected 1)"
    expect lastOutput.contains("35"),
           "body-dep: ran a stale body; expected the program to print 35"

  restoreSources()
  discard run("body-dep-settle")

  # Phase 5c: options. nifmake sees files, not flags, so the driver records
  # the options a stage's artifacts were made with (`<nimcache>/config.nif`
  # for the frontend, `<nimcache>/<backend>/<main>/config.nif` for the
  # backend) and reruns the stage when they differ. A define reaches sem, so
  # both stages rerun; `--opt` only the backend. Repeating a build changes
  # nothing, and neither does going back to the options it started with
  # but once.
  block:
    proc withOptions(opts: string): string =
      nimony.quoteShell & " c -r " & opts & " --silentMake --report --nimcache:" &
        cache.quoteShell & " " & src.quoteShell
    var r = run("define", withOptions("-d:incrementalProbe"))
    if r.len == 2:
      expect reportField(r[0], "nimsem") >= 1, "define: nimsem did not re-run"
      expect reportField(r[1], "cc") >= 1, "define: the backend did not re-run"
    r = run("define-settle", withOptions("-d:incrementalProbe"))
    if r.len == 2:
      expect reportField(r[0], "total") == 0 and reportField(r[1], "total") == 0,
             "define-settle: re-ran commands"
    r = run("opt", withOptions("-d:incrementalProbe --opt:size"))
    if r.len == 2:
      expect reportField(r[0], "total") == 0,
             "opt: the frontend re-ran " & $reportField(r[0], "total") & " commands"
      expect reportField(r[1], "cc") >= 1, "opt: the C compiler did not re-run"
    r = run("opt-settle", withOptions("-d:incrementalProbe --opt:size"))
    if r.len == 2:
      expect reportField(r[0], "total") == 0 and reportField(r[1], "total") == 0,
             "opt-settle: re-ran commands"
    r = run("options-back")
    if r.len == 2:
      expect reportField(r[0], "nimsem") >= 1, "options-back: nimsem did not re-run"
      expect reportField(r[1], "cc") >= 1, "options-back: the backend did not re-run"
    r = run("options-back-settle")
    if r.len == 2:
      expect reportField(r[0], "total") == 0 and reportField(r[1], "total") == 0,
             "options-back-settle: re-ran commands"

  # The DCE files, on a cache of their own: the edits above keep the backend
  # from settling. A local shifts every `SymId` after it, and no live file may
  # move; then `sample` calls `livedep.unusedProc`, and only `livedep`'s may.
  block:
    let dceCache = "nimcache" / "incremental-dce"
    removeDir dceCache
    let dceCmd = nimony.quoteShell & " c -r --silentMake --report --nimcache:" &
                 dceCache.quoteShell & " " & src.quoteShell
    discard run("dce-cold", dceCmd)
    let edited = originalDep.replace("  x + 1\n", "  let step = 1\n  x + step\n")
    expect edited != originalDep, "dce-local: the edit no longer applies to " & dep
    writeFile(dep, edited)
    var r = run("dce-local", dceCmd)
    if r.len == 2:
      # `dceLive` does rerun: its inputs are the `.x.nif`s that carry the
      # analysis, and the edited module's did change. What must not happen is
      # that it moves a live file — so only the modules whose `.x.nif` was
      # remade re-emit: the edited one, and `sample`, whose hexer re-ran
      # because `bump` is `.inline` and so part of the edited module's
      # interface.
      expect reportField(r[1], "dceEmit") <= 2,
             "dce-local: dceEmit ran " & $reportField(r[1], "dceEmit") & " times (expected at most 2)"
    r = run("dce-local-settle", dceCmd)
    if r.len == 2:
      expect reportField(r[1], "dceEmit") == 0,
             "dce-local-settle: dceEmit ran " & $reportField(r[1], "dceEmit") & " times (expected 0)"

    let liveFiles = filesBySuffix(dceCache, ".live.nif")
    let modules = filesBySuffix(dceCache, ".x.nif").len
    expect liveFiles.len == modules,
           "live-edit: " & $liveFiles.len & " live files for " & $modules & " modules"
    let before = liveFiles.mapIt(readFile(it))
    writeFile(src, originalSrc & "\necho unusedProc(10)\n")
    r = run("live-edit", dceCmd)
    expect lastOutput.splitLines.anyIt(it.strip == "20"),
           "live-edit: the program did not print '20'"
    var moved = 0
    for i, f in liveFiles:
      let now = readFile(f)
      if now != before[i]:
        inc moved
        expect "unusedProc" in now and "unusedProc" notin before[i],
               "live-edit: " & f & " moved without `unusedProc` becoming live"
    expect moved == 1, "live-edit: " & $moved & " live files moved, expected 1"
    if r.len == 2:
      let emits = reportField(r[1], "dceEmit")
      let most = moved + reportField(r[1], "hexer")
      expect emits in 1 .. most,
             "live-edit: dceEmit ran " & $emits & " times, expected 1 .. " & $most

  restoreSources()

  # Phase 6: switch backends without touching a source file. `nimony c` and
  # `nimony n` do not produce the same artifacts from the same input — hexer
  # alone runs with or without `--native`, which changes the main module's
  # `.x.nif` — and nifmake reruns a node only when its input or output FILES
  # changed, never when a tool's FLAGS did. So sharing one directory would not
  # make the second backend overwrite the first: it would make it reuse the
  # first one's artifacts, silently, for as long as the sources hold still.
  # `deps.backendDirName` keeps the two populations apart; assert that both
  # exist afterwards, that they disagree, and that the C build the native one
  # ran on top of came through untouched.
  var phases = 9  # cold, noop, touch, edit, inline-dep, body-dep, options, dce-local, live-edit
  let arkham = "bin" / "arkham".addFileExt(ExeExt)
  let nifasm = "bin" / "nifasm".addFileExt(ExeExt)
  if fileExists(arkham) and fileExists(nifasm):
    inc phases
    block:
      let before = mainHexedPerBackend(cache)
      expect before.len == 1,
             "backend-switch: expected 1 backend directory before, got " & $before.len
      let nativeCmd = nimony.quoteShell & " n -r --silentMake --report --nimcache:" &
                      cache.quoteShell & " " & src.quoteShell
      let (nativeOut, nativeEc) = execCmdEx(nativeCmd)
      if nativeEc != 0:
        stdout.write nativeOut
        restoreSources()
        quit "incremental: 'backend-switch' native compile failed"
      let after = mainHexedPerBackend(cache)
      expect after.len == 2,
             "backend-switch: expected a directory per backend, got " & $after.len &
             " (" & after.mapIt(it[0]).join(", ") & ")"
      if after.len == 2 and before.len == 1:
        expect after[0][1] != after[1][1],
               "backend-switch: both backends stored the same main .x.nif"
        let cBefore = after.filterIt(it[0] == before[0][0])
        expect cBefore.len == 1 and cBefore[0][1] == before[0][1],
               "backend-switch: the native build rewrote the C backend's main .x.nif"

  # Phases 7-11: files read at COMPILE TIME that no source file mentions —
  # what a `.plugin` reports through `plugins.dependsOn` and what `slurp`
  # folds (nim-lang/nimony#1378). Nothing in the module's own inputs changes
  # when such a file is edited, so without the `(dependency …)` bookkeeping
  # the `.s.nif` looks current forever and the program keeps printing the old
  # contents. Driven from a fixture of its own so the plugin build node does
  # not perturb the counts asserted above.
  let depSrc = "tests/incremental/plugindep.nim"
  let pluginData = "tests/incremental/plugindata.txt"
  let slurpData = "tests/incremental/slurpdata.txt"
  let depCache = "nimcache" / "incremental-deps"
  if fileExists(depSrc) and fileExists(pluginData) and fileExists(slurpData):
    phases += 5
    let originalPluginData = readFile(pluginData)
    let originalSlurpData = readFile(slurpData)
    removeDir depCache
    let depCmd = nimony.quoteShell & " c -r --silentMake --report --nimcache:" &
                 depCache.quoteShell & " " & depSrc.quoteShell

    var depOutput = ""
    proc runDep(label: string): seq[seq[ReportEntry]] =
      let (output, ec) = execCmdEx(depCmd)
      depOutput = output
      if ec != 0:
        stdout.write output
        writeFile(pluginData, originalPluginData)
        writeFile(slurpData, originalSlurpData)
        restoreSources()
        quit "incremental: '" & label & "' compile failed"
      parseNifmakeReports(output)

    # Phase 7: cold build establishes the baseline and records both files.
    block:
      discard runDep("dep-cold")
      expect depOutput.contains("plugin-one"),
             "dep-cold: plugin did not read its data file"
      expect depOutput.contains("slurp-one"),
             "dep-cold: slurp did not read its data file"

    # Phase 8: nothing changed — the extra inputs must not make the node
    # perpetually stale.
    block:
      let r = runDep("dep-noop")
      if r.len == 2:
        expect reportField(r[0], "total") == 0,
               "dep-noop: frontend re-ran " & $reportField(r[0], "total") & " commands"
        expect reportField(r[1], "total") == 0,
               "dep-noop: backend re-ran " & $reportField(r[1], "total") & " commands"

    # Phase 9: edit the file the PLUGIN reads. Two caches have to give way —
    # nifmake's (the module is re-semmed at all) and `runPlugin`'s memo of the
    # plugin output, which is keyed on the input tree and so did not change.
    block:
      writeFile(pluginData, "plugin-two")
      let r = runDep("dep-plugin-edit")
      if r.len == 2:
        expect reportField(r[0], "nimsem") >= 1,
               "dep-plugin-edit: nimsem did not re-run"
      expect depOutput.contains("plugin-two"),
             "dep-plugin-edit: ran a stale plugin expansion; expected 'plugin-two'"

    # Phase 10: same for the file `slurp` folded.
    block:
      writeFile(slurpData, "slurp-two")
      let r = runDep("dep-slurp-edit")
      if r.len == 2:
        expect reportField(r[0], "nimsem") >= 1,
               "dep-slurp-edit: nimsem did not re-run"
      expect depOutput.contains("slurp-two"),
             "dep-slurp-edit: folded a stale slurp; expected 'slurp-two'"

    # Phase 11: DELETE the plugin's data file. It cannot be listed as a
    # nifmake input any more, so the re-sem is forced by dropping the output
    # instead — and that must happen exactly once. A run that keeps forcing it
    # is the failure mode this phase exists to catch: the re-sem no longer
    # records the file, so the build after it has to be a clean no-op.
    block:
      removeFile(pluginData)
      discard runDep("dep-delete")
      expect depOutput.contains("plugin-data-missing"),
             "dep-delete: kept a stale plugin expansion after the data file vanished"
      let r = runDep("dep-delete-settle")
      if r.len == 2:
        expect reportField(r[0], "total") == 0,
               "dep-delete-settle: frontend re-ran " & $reportField(r[0], "total") &
               " commands; a missing dependency must force ONE rebuild, not a loop"
    writeFile(pluginData, originalPluginData)
    writeFile(slurpData, originalSlurpData)

  let dt = epochTime() - t0
  if failures.len > 0:
    for f in failures: stderr.writeLine "incremental: " & f
    quit "FAILURE: " & $failures.len & " incremental phase(s) failed."
  echo "incremental: ", phases, " / ", phases, " phases successful in ",
       formatFloat(dt, ffDecimal, precision=2), "s."
  echo "SUCCESS."
