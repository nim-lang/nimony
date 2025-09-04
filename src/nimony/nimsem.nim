#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nimony semantic checker.

import std / [parseopt, sets, strutils, os, assertions, syncio]

import ".." / hexer / hexer # only imported to ensure it keeps compiling
import ".." / gear2 / modnames
import ".." / lib / argsfinder
import sem, nifconfig, semos, semdata, indexgen
import nifstreams, derefs, deps, nifcursors, nifreader, nifbuilder

const
  Version = "0.2"
  Usage = "Nimsem Semantic Checker. Version " & Version & """

  (c) 2024-2025 Andreas Rumpf
Usage:
  nimsem [options] [command]
Command:
  m file.nim [project.nim]    compile a single Nim module to hexer
  x file.nif                  generate the .idx.nif file from a .nif file
  e file.nif [dep1.nif ...]   execute the given .nif file

Options:
  -d, --define:SYMBOL       define a symbol for conditional compilation
  -p, --path:PATH           add PATH to the search path
  --compat                  turn on compatibility mode
  --isSystem                passed module is a `system.nim` module
  --isMain                  passed module is the main module of a project
  --noSystem                do not auto-import `system.nim`
  --bits:N                  `int` has N bits; possible values: 64, 32, 16
  --cpu:SYMBOL              set the target processor (cross-compilation)
  --os:SYMBOL               set the target operating system (cross-compilation)
  --base:PATH               set the base directory for the configuration system
  --nimcache:PATH           set the path used for generated files
  --flags:FLAGS             undocumented flags
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

type
  Command = enum
    None, SingleModule, GenerateIdx, Execute

proc singleModule(infile, outfile, idxfile: string; config: sink NifConfig; moduleFlags: set[ModuleFlag]) =
  if not semos.fileExists(infile):
    quit "cannot find " & infile
  else:
    semcheck(infile, outfile, ensureMove config, moduleFlags, "", false)

proc executeNif(files: seq[string]; config: sink NifConfig) =
  # file 0 is special as it is the main file. We need to run injectDerefs on it first.
  # The other modules are simply dependencies we need to compile&link too.
  if files.len == 0:
    return

  let dependencyFiles = files[1..^1]

  # Step 1: Run injectDerefs on the main file
  var stream = nifstreams.open(files[0])
  defer: nifstreams.close(stream)

  discard processDirectives(stream.r)
  var buf = fromStream(stream)
  let mainCursor = beginRead(buf)

  # Transform the main file with injectDerefs
  let transformedMain = injectDerefs(mainCursor)

  # Write the transformed main file to a temporary location
  let transformedMainFile = config.nifcachePath / "transformed_main.nif"
  writeFile(transformedMainFile, "(.nif24)\n" & toString(transformedMain))

  # Step 2: Use the existing deps.nim infrastructure to build from .nif files
  buildGraphFromNif(
    config = config,
    mainNifFile = transformedMainFile,
    dependencyNifFiles = dependencyFiles,
    forceRebuild = false,
    silentMake = false,
    moduleFlags = {}
  )

proc handleCmdLine() =
  var args: seq[string] = @[]
  var cmd = Command.None
  var forceRebuild = false
  var moduleFlags: set[ModuleFlag] = {}
  var config = initNifConfig("")
  var commandLineArgs = ""
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if cmd == None:
        case key.normalize:
        of "m":
          cmd = SingleModule
        of "x":
          cmd = GenerateIdx
        of "e":
          cmd = Execute
        else:
          quit "command expected"
      else:
        args.add key

    of cmdLongOption, cmdShortOption:
      var forwardArg = true
      case normalize(key)
      of "base": config.baseDir = val
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      of "forcebuild", "f", "ff": forceRebuild = true
      of "compat": config.compat = true
      of "path", "p": config.paths.add val
      of "define", "d": config.defines.incl val
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
      of "cc":
        config.cc = val
        config.ccKey = extractCCKey(val)
      of "linker":
        config.linker = val
      of "nimcache":
        config.nifcachePath = val
      else: writeHelp()
      if forwardArg:
        commandLineArgs.add " --" & key
        if val.len > 0:
          commandLineArgs.add ":" & quoteShell(val)

    of cmdEnd: assert false, "cannot happen"
  semos.setupPaths(config)
  case cmd
  of None:
    quit "command missing"
  of SingleModule:
    if args.len != 3:
      quit "want exactly 3 command line arguments"
    singleModule(args[0], args[1], args[2], ensureMove config, moduleFlags)
  of GenerateIdx:
    if args.len != 1:
      quit "want exactly 1 command line argument"
    indexFromNif(args[0])
  of Execute:
    if args.len == 0:
      quit "want more than 0 command line argument"
    executeNif args, ensureMove config

when isMainModule:
  handleCmdLine()
