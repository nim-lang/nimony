#       Nifler
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## `nifler scan`: parse a module and everything it transitively imports or
## includes, in one process.
##
## The driver (`nimony/deps.nim`) used to discover the module graph by spawning
## one `nifler parse` per module. Parsing is cheap — about 1.5 µs per line — but
## a process spawn costs ~2.7 ms, so a 26-module graph spent ~70 ms almost
## entirely on `fork`/`exec`. `scan` performs the same walk in-process and
## leaves the exact same `.p.nif` / `.p.deps.nif` artifacts behind, after which
## the driver's own scan finds everything up to date and spawns nothing.
##
## `scan` is a *prefetcher*, not an authority: `deps.nim` still walks the graph
## itself and still decides what needs rebuilding. That is what lets this module
## take the cheap route everywhere — it ignores `when` conditions and simply
## parses both branches' imports, silently skips anything that fails to resolve
## or to parse, and never reports an error. Over-approximating costs a stray
## parse; under-approximating costs one `nifler` spawn in the driver, exactly as
## before. Neither can change the build's outcome.

import std / [os, sets, strutils]
import bridge
import ".." / lib / modresolve
import ".." / gear2 / modnames

include ".." / lib / compat2

type
  ScanConfig* = object
    paths*: seq[string]
    nifcachePath*: string
    preserveDocs*: bool

proc parsedFile(c: ScanConfig; nimFile: string): string =
  ## Must agree with `deps.parsedFile`, or the driver won't see our artifacts.
  c.nifcachePath / moduleSuffix(nimFile, c.paths) &
    (if c.preserveDocs: ".pc.nif" else: ".p.nif")

proc scanFile(c: ScanConfig; nimFile: string; seen: var HashSet[string]) =
  if seen.containsOrIncl(nimFile): return
  var refs: seq[string] = @[]
  # Always re-parse rather than consulting mtimes: the parse is the cheap part
  # and `nifbuilder`'s OnlyIfChanged mode keeps the output's mtime when the
  # result is byte-identical, so the driver's freshness checks still behave
  # exactly as they did. Skipping would mean recovering the module's imports
  # from the cached `.p.deps.nif`, i.e. a second, divergence-prone reader for
  # something we can read off the AST for free.
  if not parseFile(nimFile, c.parsedFile(nimFile), portablePaths = true,
                   depsEnabled = true, depsOnly = false,
                   preserveDocs = c.preserveDocs, moduleRefs = addr refs,
                   bailOnError = false):
    return
  for r in refs:
    let dep = resolveFileWrapper(c.paths, nimFile, r)
    if fileExists(dep):
      scanFile(c, dep, seen)

proc scanProject*(c: ScanConfig; root: string; skipSystem: bool) =
  ## `root` is a `.nim` file; `.nif` inputs (compile-time eval snippets) have
  ## nothing to parse.
  if root.endsWith(".nif") or not fileExists(root): return
  if c.nifcachePath.len > 0:
    createDir c.nifcachePath
  var seen = initHashSet[string]()
  scanFile(c, root, seen)
  if not skipSystem:
    # Every module gets `system` implicitly, so the driver always walks it.
    let sys = resolveFileWrapper(c.paths, root, "std/system")
    if fileExists(sys):
      scanFile(c, sys, seen)
