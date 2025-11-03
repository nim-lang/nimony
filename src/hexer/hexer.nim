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
- Lower control flow expressions to control flow statements (elminate the expr/nkStmtListExpr construct).
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
import ".." / nimony / [langmodes]
import nifcgen, lifter, duplifier, destroyer, inliner, constparams, dce2

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
  --flags:FLAGS             undocumented flags
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc handleCmdLine*() =
  var files: seq[string] = @[]
  var bits = sizeof(int) * 8
  var flags = DefaultSettings
  var action = ""
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
      expand files[0], bits, flags
    of "d":
      deadCodeElimination files
    else:
      writeHelp()

when isMainModule:
  handleCmdLine()
