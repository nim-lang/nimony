#
#
#           Shoggoth Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The compiler's middle and back end in one binary: hexer (Nimony NIF ->
## Leng lowering plus dead code elimination) and the optional NIFC tree
## optimizer. The optimizer itself lives in `optdriver` (built on **nifcore**);
## `optdriver.processFile` reads a NIFC module, runs the inter-module inliner
## (via `imi_bridge`) over the whole module, then the per-body passes, and
## writes the result. It runs only when the build asks for it (`--opt:speed` /
## `--opt:size`), as its own `opt` step after `de`.
##
## Subcommands:
##
##   shoggoth c file.nif                 compile a semchecked NIF file to Leng
##   shoggoth d file1.nif file2.nif ...  dead code elimination for the given files
##   shoggoth dl <x.nif>...              compute the global live sets
##   shoggoth de <x.nif> <live.nif>      per-module emit of the live code
##   shoggoth opt [--outdir:DIR] [--verify] [--stats] <input.nif> [<output.nif>]
##       optimize NIFC modules. With no `<output>` (and no `--outdir`) each
##       input is rewritten in place.
##   shoggoth pat [--from:NIF] [--keep] [--shoggoth] <file.nim> [<substr>]
##       pattern-by-example: compile a .nim with nimony and print its NIFC procs.

import std / [os, strutils, syncio, dirs, paths, parseopt, assertions]
import ".." / ".." / nimony / [langmodes, nifconfig]
import ".." / ".." / hexer / hexer
import ".." / ".." / lib / [vfs, nimversion]
import optdriver    # processFile / Stats — keeps its nifcore world isolated
import patextract   # patMain — likewise nifcore-isolated
when defined(cseSummaryStats):
  import cse

include ".." / ".." / "lib" / compat2   # onRaiseQuit, path()

const
  Usage = "Shoggoth Compiler. Version " & Version & """

  (c) 2024-2026 Andreas Rumpf
Usage:
  shoggoth [options] [command]
Command:
  c file.nif                compile semchecked NIF file to Leng
  d file1.nif file2.nif ... perform dead code elimination for the given NIF files
  dl file1.x.nif ...        compute the global live set (one .live.nif per module)
  de file.x.nif file.live.nif
                            emit one module's live code as .c.nif
  opt [--outdir:DIR] [--verify] [--stats] [--vectorize[:sse]] in.nif [out.nif]
                            optimize NIFC modules (in place without out.nif)
  pat [--from:NIF] [--keep] [--shoggoth] file.nim [substr]
                            compile a .nim file and print its NIFC procs

Options:
  --bits:N                  `int` has N bits; possible values: 64, 32, 16
  --os:NAME                 target operating system (default: the host's)
  --outdir:DIR              (d, dl, de) write the outputs to DIR
  --isMain                  mark the file as the main module
  --native                  target the native backend (arkham+nifasm, no C)
  --app:TYPE                application type: console, gui, lib, staticlib (default: console)
  --flags:FLAGS             undocumented flags
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc hexerMain() =
  ## The hexer commands (`c`, `d`, `dl`, `de`), driven by the whole command line.
  var files: seq[string] = @[]
  var bits = sizeof(int) * 8
  var bigEndian = false
  var flags = DefaultSettings
  var outdir = ""
  var action = ""
  var isMain = false
  var native = false
  var crt = false
  var cycles = false
  var appType = appConsole
  var isWindows = defined(windows)
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if action.len == 0:
        action = key.normalize
      else:
        files.add key
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "bits":
        case val
        of "64": bits = 64
        of "32": bits = 32
        of "16": bits = 16
        else: quit "invalid value for --bits"
      of "cpu":
        case val
        of "be": bigEndian = true
        of "le": bigEndian = false
        else: quit "invalid value for --cpu; expected 'be' or 'le'"
      of "os":
        # Only the Windows/not-Windows distinction reaches the code generator:
        # a Windows entry point receives no argc/argv/envp (see `genMainProc`).
        isWindows = normalize(val) == "windows"
      of "outdir":
        outdir = val
      of "ismain":
        isMain = true
      of "native":
        native = true
      of "crt":
        crt = true
      of "cycles":
        cycles = true
      of "app":
        case normalize(val)
        of "console": appType = appConsole
        of "gui": appType = appGui
        of "lib": appType = appLib
        of "staticlib": appType = appStaticLib
        else: quit "invalid value for --app; expected console, gui, lib, or staticlib"
      of "flags":
        flags = parseFlags(val)
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      else: writeHelp()
    of cmdEnd: assert false, "cannot happen"
  if action == "c" and files.len > 1:
    quit "too many arguments given, seek --help"
  elif action.len == 0 or files.len == 0:
    writeHelp()
  else:
    case action
    of "c":
      expand files[0], bits, bigEndian, flags, isMain, outdir, appType, native, isWindows, crt,
             cycles
    of "d":
      deadCodeElimination(files, outdir)
    of "dl":
      # Compute the global live set + resolve table from the `(dce …)` section
      # each given `.x.nif` carries; outputs <outdir>/<M>.live.nif for each of
      # them.
      computeLiveSet(files, outdir)
    of "de":
      # Per-module emit. Args: <M.x.nif> <M.live.nif>; outputs
      # <outdir>/<M>.c.nif.
      if files.len != 2:
        quit "de: expected <x.nif> <live.nif>"
      dceEmit(files[0], files[1], outdir)
    else:
      writeHelp()

proc runOne(input, output: string; verify, stats: bool; vecMode: VecMode) =
  let st = processFile(input, output, verify, vecMode)
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
  var vecMode = vecOff
  for a in args:
    if a.startsWith("--outdir:"): outdir = a["--outdir:".len .. ^1]
    elif a == "--verify": verify = true
    elif a == "--stats": stats = true
    elif a == "--vectorize": vecMode = vecNeon
    elif a == "--vectorize:sse": vecMode = vecSse
    elif a.startsWith("-"): quit "unknown option: " & a
    else: positional.add a

  if positional.len == 0:
    quit "usage: shoggoth opt [--outdir:DIR] [--verify] [--stats] <input.nif> [<output.nif>]"

  if outdir.len > 0:
    onRaiseQuit createDir(path(outdir))
    for inp in positional:
      runOne(inp, outdir / extractFilename(inp), verify, stats, vecMode)
  elif positional.len == 2:
    runOne(positional[0], positional[1], verify, stats, vecMode)
  else:
    # in-place rewrite of each input
    for inp in positional:
      runOne(inp, inp, verify, stats, vecMode)

proc main =
  # `commandLineParams` rather than `paramStr`: Nimony's `parseopt` exports a
  # `paramStr`/`paramCount` of its own, which clash with `os`'s.
  let args = commandLineParams()
  var sub = ""
  if args.len > 0: sub = args[0]
  case sub
  of "opt", "pat":
    var rest: seq[string] = @[]
    for i in 1 ..< args.len: rest.add args[i]
    if sub == "opt": optimizeMain(rest)
    else: patMain(rest)
  else:
    hexerMain()

main()
dumpVfsProfile("shoggoth")

when defined(cseSummaryStats):
  stderr.writeLine "[cse] calls=", cse.gCallsSeen,
                   " foreignFound=", cse.gForeignFound,
                   " foreignMissing=", cse.gForeignMissing,
                   " noReturnSaved=", cse.gNoReturnSaved,
                   " clearUnknown=", cse.gClearUnknown,
                   " clearGlobal=", cse.gClearGlobal, " entriesKeptAcrossCalls=", cse.gEntriesKept
  stderr.writeLine "[cse] pathTests=", cse.gPathTests,
                   " pathSavedStore=", cse.gPathSavedStore,
                   " pathSavedCall=", cse.gPathSavedCall
