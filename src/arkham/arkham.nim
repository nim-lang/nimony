#
#           Arkham — native AArch64 code generator for NIFC
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution.
#

## arkham translates a single NIFC `.c.nif` file into typed `nifasm` NIF
## (AArch64 / Darwin), which `nifasm` then type-checks, assembles and links.

import std / [parseopt, syncio, strutils]
import nifcoreparse              # parseFromFile + nifcore
import nifcdecl                  # createNifcTagPool
import codegen                   # the actual translation

const
  Version = "0.1.0"
  Usage = """arkham — native AArch64 code generator for NIFC """ & Version & """

Usage:
  arkham [options] file.c.nif

Options:
  -o:file, --output:file   output asm-NIF file (default: <input>.asm.nif)
  -h, --help               show this help
"""

proc run(input, output: string) =
  var buf = parseFromFile(input, sharedTags = createNifcTagPool())
  writeFile(output, generate(buf))

proc main() =
  var input, output = ""
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if input.len == 0: input = key
    of cmdLongOption, cmdShortOption:
      case key.normalize
      of "output", "o": output = val
      of "help", "h": quit(Usage, QuitSuccess)
    of cmdEnd: discard
  if input.len == 0: quit(Usage, QuitSuccess)
  if output.len == 0: output = input & ".asm.nif"
  run(input, output)

when isMainModule:
  main()
