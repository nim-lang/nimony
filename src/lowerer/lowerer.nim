#
#
#           lowerer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#


import std / [parseopt, strutils, os, osproc, tables, assertions, syncio]
import transformer

const
  Version = "0.2"
  Usage = "lowerer Compiler. Version " & Version & """

  (c) 2024 Andreas Rumpf
Usage:
  lowerer [options] [command]
Command:
  file.nif      lower NIF file to meet gear3's requirements

Options:
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc handleCmdLine*() =
  var files: seq[string] = @[]
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      files.add key
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      else: writeHelp()
    of cmdEnd: assert false, "cannot happen"
  if files.len > 1:
    quit "too many arguments given, seek --help"
  elif files.len == 0:
    writeHelp()
  else:
    transformCode files[0]

when isMainModule:
  handleCmdLine()
