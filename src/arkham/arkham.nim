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
import codegen_a64               # AArch64 / Darwin backend
import codegen_x64               # x86-64 / Linux backend

const
  Version = "0.1.0"
  Usage = """arkham — native code generator for NIFC """ & Version & """

Usage:
  arkham [options] file.c.nif

Options:
  -o:file, --output:file   output asm-NIF file (default: <input>.asm.nif)
  -a:arch, --arch:arch     target: arm64 (default) | x64
  -h, --help               show this help
"""

proc run(input, output, arch: string) =
  # One shared tag pool across the main module and any foreign modules the
  # program model loads on demand, so tag ordinals (hence stmtKind/typeKind
  # decoding) line up across modules.
  let tags = createNifcTagPool()
  var buf = parseFromFile(input, sharedTags = tags)
  let code = case arch
             of "x64", "x86_64", "amd64": generateX64(buf, input, tags)
             of "arm64", "aarch64", "": generateA64(buf, input, tags)
             else: quit("arkham: unknown --arch:" & arch, QuitFailure)
  writeFile(output, code)

proc main() =
  var input, output, arch = ""
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if input.len == 0: input = key
    of cmdLongOption, cmdShortOption:
      case key.normalize
      of "output", "o": output = val
      of "arch", "a": arch = val
      of "help", "h": quit(Usage, QuitSuccess)
    of cmdEnd: discard
  if input.len == 0: quit(Usage, QuitSuccess)
  if output.len == 0: output = input & ".asm.nif"
  run(input, output, arch)

when isMainModule:
  main()
