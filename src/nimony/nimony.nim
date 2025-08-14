#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nimony driver program.

when defined(windows):
  when defined(gcc):
    when defined(x86):
      {.link: "../../icons/nimony.res".}
    else:
      {.link: "../../icons/nimony_icon.o".}

import std / [parseopt, sets, strutils, os, assertions, syncio]
import ".." / lib / [tooldirs, argsfinder]

import ".." / hexer / hexer # only imported to ensure it keeps compiling
import ".." / gear2 / modnames
import sem, nifconfig, semos, semdata, deps, langmodes

const
  Version = "0.2"
  Usage = "Nimony Compiler. Version " & Version & """

  (c) 2024-2025 Andreas Rumpf
Usage:
  nimony [options] [command]
Command:
  c project.nim               compile the full project
  m file.nim [project.nim]    compile a single Nim module to hexer

Options:
  -d, --define:SYMBOL       define a symbol for conditional compilation
  -p, --path:PATH           add PATH to the search path
  -f, --forcebuild          force a rebuild
  --ff                      force a full build
  -r, --run                 also run the compiled program
  --compat                  turn on compatibility mode
  --isSystem                passed module is a `system.nim` module
  --isMain                  passed module is the main module of a project
  --noSystem                do not auto-import `system.nim`
  --bits:N                  `int` has N bits; possible values: 64, 32, 16
  --cpu:SYMBOL              set the target processor (cross-compilation)
  --os:SYMBOL               set the target operating system (cross-compilation)
  --silentMake              suppresses make output
  --nimcache:PATH           set the path used for generated files
  --boundchecks:on|off      turn bound checks on or off
  --cc:C_COMPILER           set the C compiler
  --linker:LINKER           set the linker
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc processSingleModule(nimFile: string; config: sink NifConfig; moduleFlags: set[ModuleFlag];
                         commandLineArgs: string; forceRebuild: bool) =
  let nifler = findTool("nifler")
  let name = moduleSuffix(nimFile, config.paths)
  let src = config.nifcachePath / name & ".1.nif"
  let dest = config.nifcachePath / name & ".2.nif"
  let toforceRebuild = if forceRebuild: " -f " else: ""
  exec quoteShell(nifler) & " --portablePaths p " & toforceRebuild & quoteShell(nimFile) & " " &
    quoteShell(src)
  semcheck(src, dest, ensureMove config, moduleFlags, commandLineArgs, true)

type
  Command = enum
    None, SingleModule, FullProject

proc dispatchBasicCommand(key: string): Command =
  case key.normalize:
  of "m":
    SingleModule
  of "c":
    FullProject
  else:
    quit "command expected"

proc determineBaseDir(): string =
  # in a prepass determine the base directory for the configuration system:
  var cmd = Command.None
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if cmd == None: cmd = dispatchBasicCommand(key)
      else:
        return val.splitFile.dir
    else: discard
  return ""

proc handleCmdLine() =
  var args: seq[string] = @[]
  var cmd = Command.None
  var forceRebuild = false
  var fullRebuild = false
  var silentMake = false
  var doRun = false
  var moduleFlags: set[ModuleFlag] = {}
  var config = initNifConfig()
  var commandLineArgs = ""
  var commandLineArgsNifc = ""
  var isChild = false
  var passC = ""
  var passL = ""
  var checkModes = DefaultSettings
  var forwardArgsToExecutable = false
  var executableArgs = ""
  config.baseDir = determineBaseDir()

  when false:
    # XXX To implement
    if config.baseDir.len > 0:
      let argsFile = findArgs(config.baseDir, "nimony.args")
      processArgsFile argsFile, args

  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if cmd == None:
        cmd = dispatchBasicCommand(key)
      else:
        if forwardArgsToExecutable:
          executableArgs.add " " & quoteShell(key)
        else:
          args.add key
          if cmd == FullProject and doRun and args.len >= 1:
            forwardArgsToExecutable = true

    of cmdLongOption, cmdShortOption:
      if forwardArgsToExecutable:
        executableArgs.add " --" & key
        if val.len > 0:
          executableArgs.add ":" & quoteShell(val)
      else:
        var forwardArg = true
        var forwardArgNifc = false
        case normalize(key)
        of "help", "h": writeHelp()
        of "version", "v": writeVersion()
        of "forcebuild", "f": forceRebuild = true
        of "ff":
          fullRebuild = true
          forceRebuild = true
        of "run", "r":
          doRun = true
          if cmd == FullProject and args.len >= 1:
            forwardArgsToExecutable = true
          forwardArg = false
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
        of "boundchecks":
          forwardArg = false
          case val
          of "on": checkModes.incl BoundCheck
          of "off": checkModes.excl BoundCheck
          else: quit "invalid value for --boundchecks"
        of "silentmake":
          silentMake = true
          forwardArg = false
        of "ischild":
          # undocumented command line option, by design
          isChild = true
          forwardArg = false
        of "passc":
          if passC.len > 0:
            passC.add " "
          passC.add val
          forwardArg = false
        of "passl":
          if passL.len > 0:
            passL.add " "
          passL.add val
          forwardArg = false
        of "nimcache":
          config.nifcachePath = val
          forwardArgNifc = true
        of "cc":
          config.cc = val
        of "linker":
          config.linker = val
        else: writeHelp()
        if forwardArg:
          commandLineArgs.add " --" & key
          if val.len > 0:
            commandLineArgs.add ":" & quoteShell(val)
        if forwardArgNifc:
          commandLineArgsNifc.add " --" & key
          if val.len > 0:
            commandLineArgsNifc.add ":" & quoteShell(val)

    of cmdEnd: assert false, "cannot happen"
  if args.len == 0:
    quit "too few command line arguments"
  elif args.len > 2 - int(cmd == FullProject):
    quit "too many command line arguments"

  if checkModes != DefaultSettings:
    commandLineArgs.add " --flags:" & genFlags(checkModes)

  semos.setupPaths(config)

  case cmd
  of None:
    quit "command missing"
  of SingleModule:
    if not isChild:
      createDir(config.nifcachePath)
      createDir(binDir())
      # configure required tools
      requiresTool "nifler", "src/nifler/nifler.nim", fullRebuild
      requiresTool "nifc", "src/nifc/nifc.nim", fullRebuild
    processSingleModule(args[0].addFileExt(".nim"), config, moduleFlags,
                        commandLineArgs, forceRebuild)
  of FullProject:
    createDir(config.nifcachePath)
    createDir(binDir())
    # configure required tools
    updateCompilerGitSubmodules(config)
    requiresTool "nifler", "src/nifler/nifler.nim", fullRebuild
    requiresTool "nimsem", "src/nimony/nimsem.nim", fullRebuild
    requiresTool "hexer", "src/hexer/hexer.nim", fullRebuild
    requiresTool "nifc", "src/nifc/nifc.nim", fullRebuild
    requiresTool "nifmake", "src/nifmake/nifmake.nim", fullRebuild
    # compile full project modules
    buildGraph config, args[0], forceRebuild, silentMake,
      commandLineArgs, commandLineArgsNifc, moduleFlags, (if doRun: DoRun else: DoCompile),
      passC, passL, executableArgs

when isMainModule:
  handleCmdLine()
