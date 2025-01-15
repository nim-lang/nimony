#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Nimony driver program.

import std / [parseopt, sets, strutils, os, assertions, syncio]

import ".." / hexer / hexer # only imported to ensure it keeps compiling
import ".." / gear2 / modnames
import sem, nifconfig, semos, semdata, deps

const
  Version = "0.2"
  Usage = "Nimony Compiler. Version " & Version & """

  (c) 2024 Andreas Rumpf
Usage:
  nimony [options] [command]
Command:
  c project.nim               compile the full project
  m file.nim [project.nim]    compile a single Nim module to hexer

Options:
  -d, --define:SYMBOL       define a symbol for conditional compilation
  -p, --path:PATH           add PATH to the search path
  -f, --forcebuild          force a rebuild
  -r, --run                 also run the compiled program
  --compat                  turn on compatibility mode
  --noenv                   do not read configuration from `NIM_*`
                            environment variables
  --isSystem                passed module is a `system.nim` module
  --isMain                  passed module is the main module of a project
  --noSystem                do not auto-import `system.nim`
  --bits:N                  `int` has N bits; possible values: 64, 32, 16
  --silentMake              suppresses make output
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc processSingleModule(nimFile: string; config: sink NifConfig; moduleFlags: set[ModuleFlag];
                         commandLineArgs: string; forceRebuild: bool) =
  let nifler = findTool("nifler")
  let name = moduleSuffix(nimFile, config.paths)
  let src = "nifcache" / name & ".1.nif"
  let dest = "nifcache" / name & ".2.nif"
  let toforceRebuild = if forceRebuild: " -f " else: ""
  exec quoteShell(nifler) & " --portablePaths p " & toforceRebuild & quoteShell(nimFile) & " " &
    quoteShell(src)
  semcheck(src, dest, ensureMove config, moduleFlags, commandLineArgs, true)

type
  Command = enum
    None, SingleModule, FullProject

proc handleCmdLine() =
  var args: seq[string] = @[]
  var cmd = Command.None
  var forceRebuild = false
  var compat = false
  var silentMake = false
  var useEnv = true
  var doRun = false
  var moduleFlags: set[ModuleFlag] = {}
  var config = NifConfig()
  config.defines.incl "nimony"
  config.bits = sizeof(int)*8
  var commandLineArgs = ""
  var isChild = false
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if cmd == None:
        case key.normalize:
        of "m":
          cmd = SingleModule
        of "c":
          cmd = FullProject
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
      of "run", "r":
        doRun = true
        forwardArg = false
      of "compat": compat = true
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
      of "silentmake":
        silentMake = true
        forwardArg = false
      of "ischild":
        # undocumented command line option, by design
        isChild = true
        forwardArg = false
      else: writeHelp()
      if forwardArg:
        commandLineArgs.add " --" & key
        if val.len > 0:
          commandLineArgs.add ":" & quoteShell(val)

    of cmdEnd: assert false, "cannot happen"
  if args.len == 0:
    quit "too few command line arguments"
  elif args.len > 2 - int(cmd == FullProject):
    quit "too many command line arguments"
  if useEnv:
    let nimPath = getEnv("NIMPATH")
    for entry in split(nimPath, PathSep):
      if entry.strip != "":
        config.paths.add entry
  case cmd
  of None:
    quit "command missing"
  of SingleModule:
    if not isChild:
      createDir("nifcache")
      createDir(binDir())
      requiresTool "nifler", "src/nifler/nifler.nim", forceRebuild
      requiresTool "nifc", "src/nifc/nifc.nim", forceRebuild
    processSingleModule(args[0].addFileExt(".nim"), config, moduleFlags,
                        commandLineArgs, forceRebuild)
  of FullProject:
    createDir("nifcache")
    createDir(binDir())
    requiresTool "nifler", "src/nifler/nifler.nim", forceRebuild
    requiresTool "nimsem", "src/nimony/nimsem.nim", forceRebuild
    requiresTool "hexer", "src/hexer/hexer.nim", forceRebuild
    requiresTool "nifc", "src/nifc/nifc.nim", forceRebuild
    buildGraph config, args[0], compat, forceRebuild, silentMake,
      commandLineArgs, moduleFlags, (if doRun: DoRun else: DoCompile)

when isMainModule:
  handleCmdLine()
