#
#
#           NIFC Tree Optimizer (driver)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## External tool that runs the NIFC tree-optimization passes over a module.
##
## The optimizer itself lives in `optdriver` (built on **nifcore**); this file
## is just the CLI. `optdriver.processFile` reads a NIFC module, runs the
## inter-module inliner (via `imi_bridge`) over the whole module, then the
## per-body passes (currently induction-variable strength reduction), and writes
## the result.
##
## Subcommands:
##
##   shoggoth c   [--outdir:DIR] [--verify] [--stats] <input.nif> [<output.nif>]
##       optimize NIFC modules (the build invokes this via `shoggoth c …`).
##   shoggoth pat [--from:NIF] [--keep] [--shoggoth] <file.nim> [<substr>]
##       pattern-by-example: compile a .nim with nimony and print its NIFC procs.
##
## With no `<output>` (and no `--outdir`) each input is rewritten in place. A
## first argument that is neither `c` nor `pat` is treated as the start of the
## `c` argument list (backward compatibility).

import std / [os, strutils]
import optdriver    # processFile / Stats — keeps its nifcore world isolated
import patextract   # patMain — likewise nifcore-isolated

proc runOne(input, output: string; verify, stats: bool) =
  let st = processFile(input, output, verify)
  if stats:
    var parts: seq[string] = @[]
    if st.intermodChanged > 0: parts.add "intermodinliner=" & $st.intermodChanged
    echo "  ", extractFilename(input), ": ", st.procs, " procs, ",
         st.bodies, " bodies",
         (if parts.len > 0: "  [" & parts.join(" ") & "]" else: "")

proc optimizeMain(args: seq[string]) =
  var positional: seq[string] = @[]
  var outdir = ""
  var verify = false
  var stats = false
  for a in args:
    if a.startsWith("--outdir:"): outdir = a["--outdir:".len .. ^1]
    elif a == "--verify": verify = true
    elif a == "--stats": stats = true
    elif a.startsWith("-"): quit "unknown option: " & a
    else: positional.add a

  if positional.len == 0:
    quit "usage: shoggoth c [--outdir:DIR] [--verify] [--stats] <input.nif> [<output.nif>]"

  if outdir.len > 0:
    createDir outdir
    for inp in positional:
      runOne(inp, outdir / extractFilename(inp), verify, stats)
  elif positional.len == 2:
    runOne(positional[0], positional[1], verify, stats)
  else:
    # in-place rewrite of each input
    for inp in positional:
      runOne(inp, inp, verify, stats)

proc main =
  var rest: seq[string] = @[]
  for i in 2 .. paramCount(): rest.add paramStr(i)
  let sub = if paramCount() >= 1: paramStr(1) else: ""
  case sub
  of "c":   optimizeMain(rest)
  of "pat": patMain(rest)
  of "":    quit "usage: shoggoth <c|pat> …"
  else:
    # back-compat: `shoggoth <input.nif> [<output.nif>]` == `shoggoth c …`
    var all: seq[string] = @[]
    for i in 1 .. paramCount(): all.add paramStr(i)
    optimizeMain(all)

main()
