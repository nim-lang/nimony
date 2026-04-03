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
import sem, nifconfig, semos, semdata, indexgen, programs, symparser
import nifstreams, derefs, deps, nifcursors, nifreader, nifbuilder, nifindexes, tooldirs, idetools, cli, langmodes

const
  Version = "0.2"
  Usage = "Nimsem Semantic Checker. Version " & Version & """

  (c) 2024-2025 Andreas Rumpf
Usage:
  nimsem [options] [command]
Command:
  m input.nif                 compile a single Nim module to hexer (output and index files derived from input name)
  x file.nif                  generate the .idx.nif file from a .nif file
  e file.nif [dep1.nif ...]   execute the given .nif file
  idetools file1.nif [file2.nif ...]  list usages and definitions

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
  --app:console|gui|lib|staticlib
                            set the application type (default: console)
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
    None, SingleModule, GenerateIdx, Execute, Idetools

proc processModules(infiles: seq[string]; config: sink NifConfig; moduleFlags: set[ModuleFlag]) =
  for infile in infiles:
    if not semos.fileExists(infile):
      quit "cannot find " & infile
  var outfiles: seq[string] = @[]
  for infile in infiles:
    outfiles.add infile.changeModuleExt(".s.nif")
  semcheck(infiles, outfiles, ensureMove config, moduleFlags, "", false)

proc executeNif(files: seq[string]; config: sink NifConfig) =
  # file 0 is special as it is the main file. We need to run injectDerefs on it first.
  # The other modules are simply dependencies we need to compile&link too.
  if files.len == 0:
    return

  # little hack: prepare our writenif dependency
  exec quoteShell(findTool("nimony")) & " c " & quoteShell(stdlibFile("std/writenif.nim"))

  let dependencyFiles = files[1..^1]

  buildGraphForEval(
    config = config,
    mainNifFile = files[0],
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
        of "idetools":
          cmd = Idetools
        else:
          quit "command expected"
      else:
        args.add key

    of cmdLongOption, cmdShortOption:
      var forwardArg = true
      var forwardArgNifc = false  # nimsem doesn't use this, but needed for parseCommonOption
      if parseCommonOption(key, val, config, moduleFlags, forwardArg, forwardArgNifc,
                          helpMsg = Usage, versionMsg = Version & "\n"):
        discard "handled by common CLI parser"
      else:
        case normalize(key)
        of "forcebuild", "f", "ff": forceRebuild = true
        else: writeHelp()
      if forwardArg:
        commandLineArgs.add " --" & key
        if val.len > 0:
          commandLineArgs.add ":" & quoteShell(val)

    of cmdEnd: assert false, "cannot happen"
  semos.setupPaths(config)
  if config.linker.len == 0 and config.cc.len > 0:
    config.linker = config.cc

  case cmd
  of None:
    quit "command missing"
  of SingleModule:
    if args.len < 1:
      quit "want at least 1 command line argument"
    processModules(args, ensureMove config, moduleFlags)
  of GenerateIdx:
    if args.len != 1:
      quit "want exactly 1 command line argument"
    indexFromNif(args[0])
  of Execute:
    if args.len == 0:
      quit "want more than 0 command line argument"
    executeNif args, ensureMove config
  of Idetools:
    if args.len == 0:
      quit "want more than 0 command line argument"
    case config.toTrack.mode
    of TrackUsages, TrackDef:
      usages(args, config)
    of TrackNone:
      quit "no --track information provided"

when isMainModule:
  handleCmdLine()
