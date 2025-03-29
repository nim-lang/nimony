#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
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
import nifcgen, lifter, duplifier, destroyer, inliner, constparams

const
  Version = "0.4"
  Usage = "Hexer Compiler. Version " & Version & """

  (c) 2024 Andreas Rumpf
Usage:
  hexer [options] [command]
Command:
  file.nif      compiler semchecked NIF file to NIFC

Options:
  --bits:N                  `int` has N bits; possible values: 64, 32, 16
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc handleCmdLine*() =
  var files: seq[string] = @[]
  var bits = sizeof(int) * 8
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      files.add key
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "bits":
        case val
        of "64": bits = 64
        of "32": bits = 32
        of "16": bits = 16
        else: quit "invalid value for --bits"
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      else: writeHelp()
    of cmdEnd: assert false, "cannot happen"
  if files.len > 1:
    quit "too many arguments given, seek --help"
  elif files.len == 0:
    writeHelp()
  else:
    expand files[0], bits

when isMainModule:
  handleCmdLine()
