#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nimony semantic checker.

import std / [parseopt, sets, strutils, os, assertions, syncio]

import ".." / hexer / hexer # only imported to ensure it keeps compiling
import ".." / gear2 / modnames
import sem, nifconfig, semos, semdata

const
  Version = "0.2"
  Usage = "Nimsem Semantic Checker. Version " & Version & """

  (c) 2024 Andreas Rumpf
Usage:
  nimsem [options] [command]
Command:
  m file.nim [project.nim]    compile a single Nim module to hexer

Options:
  -d, --define:SYMBOL       define a symbol for conditional compilation
  -p, --path:PATH           add PATH to the search path
  --compat                  turn on compatibility mode
  --noenv                   do not read configuration from `NIM_*`
                            environment variables
  --isSystem                passed module is a `system.nim` module
  --isMain                  passed module is the main module of a project
  --noSystem                do not auto-import `system.nim`
  --bits:N                  `int` has N bits; possible values: 64, 32, 16
  --cpu:SYMBOL              set the target processor (cross-compilation)
  --os:SYMBOL               set the target operating system (cross-compilation)
  --nimcache:PATH           set the path used for generated files
  --flags:FLAGS             undocumented flags
  --ignoreErr               produces dummy empty output files when got errors
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

type
  Command = enum
    None, SingleModule

proc singleModule(infile, outfile, idxfile: string; config: sink NifConfig; moduleFlags: set[ModuleFlag]; ignoreErr: bool) =
  if not semos.fileExists(infile):
    quit "cannot find " & infile
  else:
    semcheck(infile, outfile, ensureMove config, moduleFlags, "", false, ignoreErr)

proc handleCmdLine() =
  var args: seq[string] = @[]
  var cmd = Command.None
  var forceRebuild = false
  var useEnv = true
  var moduleFlags: set[ModuleFlag] = {}
  var config = initNifConfig()
  var commandLineArgs = ""
  var ignoreErr = false
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if cmd == None:
        case key.normalize:
        of "m":
          cmd = SingleModule
        else:
          quit "command expected"
      else:
        args.add key

    of cmdLongOption, cmdShortOption:
      var forwardArg = true
      case normalize(key)
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      of "forcebuild", "f": forceRebuild = true
      of "compat": config.compat = true
      of "path", "p": config.paths.add val
      of "define", "d": config.defines.incl val
      of "noenv": useEnv = false
      of "nosystem": moduleFlags.incl SkipSystem
      of "issystem":
        moduleFlags.incl IsSystem
        forwardArg = false
      of "ismain":
        moduleFlags.incl IsMain
        forwardArg = false
      of "bits":
        case val
        of "64": config.bits = 64
        of "32": config.bits = 32
        of "16": config.bits = 16
        else: quit "invalid value for --bits"
      of "cpu":
        if not config.setTargetCPU(val):
          quit "unknown CPU: " & val
      of "os":
        if not config.setTargetOS(val):
          quit "unknown OS: " & val
      of "flags":
        discard "nothing to do here yet, but forward these"
      of "nimcache":
        config.nifcachePath = val
      of "ignoreerr":
        ignoreErr = true
      else: writeHelp()
      if forwardArg:
        commandLineArgs.add " --" & key
        if val.len > 0:
          commandLineArgs.add ":" & quoteShell(val)

    of cmdEnd: assert false, "cannot happen"
  if args.len != 3:
    quit "want exactly 3 command line arguments"
  semos.setupPaths(config, useEnv)
  case cmd
  of None:
    quit "command missing"
  of SingleModule:
    singleModule(args[0], args[1], args[2], ensureMove config, moduleFlags, ignoreErr)

when isMainModule:
  handleCmdLine()
