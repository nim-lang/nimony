#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[

Hexer
------

Hexer is our middle-end. It transforms Nimony code into NIFC code. This requires
multiple different steps.

- Iterator inlining.
- Lambda lifting.
- Inject dups.
- Lower control flow expressions to control flow statements (eliminate the expr/nkStmtListExpr construct).
- Inject destructors.
- Map builtins like `new` and `+` to "compiler procs".
- Translate exception handling.


NIFC generation
~~~~~~~~~~~~~~~

- It copies used imported symbols into the current NIF file. As a fix point operation
  until no foreign symbols are left.
- `importc`'ed symbols are replaced by their `.c` variants.
- `importc`'ed symbols might lead to `(incl "file.h")` injections.
- Nim types must be translated to NIFC types.
- Types and procs must be moved to toplevel statements.


Grammar
-------

Hexer accepts Nimony's grammar.

]##

import std / [parseopt, strutils, os, osproc, tables, assertions, syncio]
import ".." / nimony / [langmodes, nifconfig]
import nifcgen, lifter, duplifier, destroyer, inliner, constparams, dce2
import ".." / lib / vfs

const
  Version = "0.2.0"
  Usage = "Hexer Compiler. Version " & Version & """

  (c) 2024-2025 Andreas Rumpf
Usage:
  hexer [options] [command]
Command:
  c file.nif                compile semchecked NIF file to NIFC
  d file1.nif file2.nif ... perform dead code elimination for the given NIF files

Options:
  --bits:N                  `int` has N bits; possible values: 64, 32, 16
  --outdir:DIR              (d only) write .c.nif outputs to DIR
  --isMain                  mark the file as the main module
  --app:TYPE                application type: console, gui, lib, staticlib (default: console)
  --flags:FLAGS             undocumented flags
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc handleCmdLine*() =
  var files: seq[string] = @[]
  var bits = sizeof(int) * 8
  var bigEndian = false
  var flags = DefaultSettings
  var outdir = ""
  var action = ""
  var isMain = false
  var appType = appConsole
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
      of "outdir":
        outdir = val
      of "ismain":
        isMain = true
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
      expand files[0], bits, bigEndian, flags, isMain, outdir, appType
    of "d":
      deadCodeElimination(files, outdir)
    of "dl":
      # Compute the global live set + resolve table from a list of
      # per-module `.dce.nif` analyses. Last argument is the output
      # `.live.nif`; all preceding arguments are the input `.dce.nif`s.
      if files.len < 2:
        quit "dl: expected <dce-file>... <live-output>"
      computeLiveSet(files.toOpenArray(0, files.len - 2), files[^1])
    of "de":
      # Per-module emit. Args: <M.x.nif> <main.live.nif>; outputs
      # <outdir>/<M>.c.nif.
      if files.len != 2:
        quit "de: expected <x.nif> <live.nif>"
      dceEmit(files[0], files[1], outdir)
    else:
      writeHelp()

when isMainModule:
  handleCmdLine()
  dumpVfsProfile("hexer")
