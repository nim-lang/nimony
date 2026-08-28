## The bootstrap tier list: every module known to compile with `nimony`, and
## the walk over it that attributes a regression to the smallest module reaching
## it.

import std / [os, osproc, strutils, times]
import context, deps

# ---------------------------------------------------------------------------
# Bootstrapping progress (see https://github.com/nim-lang/nimony/issues/1788).
#
# Each module listed here is known to compile with the `nimony c` command.
# New modules are added as tier-by-tier bootstrapping proceeds; the
# `hastur bootstrap` target walks this list to catch regressions.
# ---------------------------------------------------------------------------

const BootstrapModules = [
  # Only leaves of the current bootstrap DAG are listed: compiling each leaf
  # transitively covers every already-ported module via its imports. Adding
  # a module that is imported by something already on this list is redundant;
  # removing a module that has no importer in the list shrinks coverage.
  # Exception: modules in `RunnableBootstrapModules` stay here even if
  # imported elsewhere, because they need to be executed with `-r`.

  # Runnable leaf tests (executed with `c -r`).
  "src/lib/argsfinder.nim",
  "src/lib/bitabs.nim",

  # Tier 1/2 genuine leaves.
  "src/nimony/features.nim",
  "src/nimony/intervals.nim",
  "src/models/nifler_tags.nim",

  # Tier 5/6 leaves.
  "src/nimony/inferle.nim",
  "src/nimony/deferstmts.nim",
  "src/nimony/cli.nim",

  # Tier 10 leaves.
  "src/nimony/pragmacanon.nim",

  # Tier 12 tips still present after later tiers added.
  "src/nimony/semborrow.nim",
  "src/nimony/semuntyped.nim",
  "src/nimony/enumtostr.nim",
  "src/nimony/derefs.nim",

  # Tier 13 tips still present after later tiers added.
  "src/nimony/module_plugins.nim",
  "src/hexer/inliner.nim",
  "src/hexer/lambdalifting.nim",

  # Tier 14 tips still present after later tiers added.
  "src/hexer/cps.nim",
  "src/hexer/constparams.nim",
  "src/hexer/vtables_backend.nim",
  "src/hexer/dce2.nim",

  # Tier 17 tips. `hexer.nim` subsumes `lengcgen.nim` via its import set.
  "src/hexer/hexer.nim",
  "src/nimony/indexgen.nim",
  "src/nimony/idetools.nim",

  # Tier 18 tip. `sem.nim` subsumes contracts_njvl and exprexec via its
  # import set, so the Tier 16 leaves are implicitly covered by this entry.
  "src/nimony/sem.nim",

  # Tier 19 tip. Peer of `sem.nim`; both feed `nimony/nimsem.nim` at Tier 20.
  "src/nimony/deps.nim",

  # Tier 20 tip — the driver. Subsumes sem.nim, deps.nim, and hexer/hexer.nim
  # via its import set, so this entry alone exercises the full bootstrap DAG.
  "src/nimony/nimony.nim",

  # The Leng backend's own driver: a separate DAG tip (nothing in the nimony
  # front end imports it), covering both the C and the LLVM code generators
  # plus `lib/foreignmodules` and `lib/nifcdecl` via its import set.
  "src/lengc/lengc.nim",
]

# Modules whose `isMainModule` block should also be executed after compilation.
const RunnableBootstrapModules = [
  "src/lib/bitabs.nim",
  "src/lib/argsfinder.nim",
]

proc firstDiagnostic(output: string): string =
  ## The one line of a failed compile worth grouping failures by. Prefer a real
  ## diagnostic over the `FAILURE: <nifmake command line>` trailer, which names a
  ## per-module cache path and so is different for every module that shares a
  ## cause. Then drop the position tail arkham and nifasm append — ` at ??? (…)`
  ## and ` in proc <sym>` — for the same reason: one arkham assertion reached by
  ## six modules is one gap, and six spellings of it is a work list that lies
  ## about its own length.
  result = ""
  for line in output.splitLines():
    let s = line.strip()
    if s.len == 0: continue
    if "Error" in s or "error" in s or "AssertionDefect" in s or s.startsWith("[Error]"):
      result = s
      break
    if result.len == 0 and not s.startsWith("FAILURE:"): result = s
  if result.len == 0: result = "(no diagnostic)"
  let at = result.find(" at ???")
  if at >= 0: result.setLen at
  let inProc = result.find(" in proc ")
  if inProc >= 0: result.setLen inProc

proc tierTests*(native = false; extraArgs = "") =
  ## Compile every module on `BootstrapModules` with `bin/nimony`. Fails
  ## fast on the first regression so the offending module is obvious. On
  ## Windows the list collapses to the Tier 20 tip (`nimony.nim`) — its
  ## import set already covers every other entry, so the redundant per-leaf
  ## compiles are pure CI cost; Linux/macOS still cover every leaf.
  ##
  ## `native` drives `nimony n` (arkham + nifasm) rather than `nimony c`, which
  ## is what makes this list the ladder for bringing a new native target up.
  ## `boot` compiles three whole tools at once and reports the first of them that
  ## died, so a target with several independent gaps shows up as one opaque
  ## "stage 1 (nimsem) failed"; the tier list is a walk of the same DAG from the
  ## leaves upwards, so each gap is attributed to the SMALLEST module that
  ## reaches it and the ones that are already fine are named as such. Every
  ## module is attempted — no fail-fast — because the point of a bring-up run is
  ## the whole work list, not its first entry. Windows keeps its collapse to the
  ## tip either way: it is a CI cost decision, and a tip-only native run still
  ## reports every gap, just without attributing them.
  let nimony = binDir() / "nimony".addFileExt(ExeExt)
  if not fileExists(nimony):
    quit "bootstrap: " & nimony & " not found; run `hastur build nimony` first"
  if native:
    let missing = missingNativeTools()
    if missing.len > 0:
      quit "tiers native: " & missing.join(", ") & " not in " & binDir() &
           "; `hastur build native` builds them from " & NativenifDir
  let modules =
    when defined(windows): @["src/nimony/nimony.nim"]
    else: @BootstrapModules
  let backend = if native: "n" else: "c"
  let t0 = epochTime()
  var failed: seq[string] = @[]
  # Keyed by the first diagnostic line, so N modules tripping one arkham
  # assertion read as one gap with N witnesses rather than N separate failures.
  var gaps: seq[(string, seq[string])] = @[]
  for m in modules:
    removeDir "nimcache"
    var cmd = nimony.quoteShell & " " & backend
    if m in RunnableBootstrapModules: cmd.add " -r"
    if extraArgs.len > 0: cmd.add " " & extraArgs
    cmd.add " " & m.quoteShell
    let (output, ec) = execCmdEx(cmd)
    if ec == 0:
      echo "OK   ", m
    else:
      echo "FAIL ", m
      echo output
      failed.add m
      let first = firstDiagnostic(output)
      var seen = false
      for i in 0 ..< gaps.len:
        if gaps[i][0] == first:
          gaps[i][1].add m
          seen = true
          break
      if not seen: gaps.add (first, @[m])
  echo failed.len, " / ", modules.len, " bootstrap regressions in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if gaps.len > 0:
    # The work list: one line per distinct diagnostic, with the modules that
    # reach it. Fixing the top entry is what moves the most tiers at once.
    echo "distinct failure modes (", gaps.len, "):"
    for (diag, mods) in gaps:
      echo "  * ", diag
      echo "    reached by: ", mods.join(", ")
  if failed.len > 0:
    quit "FAILURE: bootstrap regression(s): " & failed.join(", ")
  else:
    echo "SUCCESS."
