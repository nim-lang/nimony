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
  Version = "0.2.0"
  Usage = "Nimony Compiler. Version " & Version & """

  (c) 2024-2025 Andreas Rumpf
Usage:
  nimony [options] [command]
Command:
  c project.nim               compile the full project
  check project.nim           check the full project for errors; can be
                              combined with `--usages`, `--def` for
                              editor integration
  m file.nim [project.nim]    compile a single Nim module to Hexer

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
  --usages:file,line,col    list usages of the symbol at the given position
  --def:file,line,col       list definition of the symbol at the given position
  --cc:C_COMPILER           set the C compiler; can be a path to the compiler's
                            executable or a name
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
  let src = config.nifcachePath / name & ".p.nif"
  let dest = config.nifcachePath / name & ".s.nif"
  let toforceRebuild = if forceRebuild: " -f " else: ""
  exec quoteShell(nifler) & " --portablePaths p " & toforceRebuild & quoteShell(nimFile) & " " &
    quoteShell(src)
  semcheck(src, dest, ensureMove config, moduleFlags, commandLineArgs, true)

type
  Command = enum
    None, SingleModule, FullProject, CheckProject

proc dispatchBasicCommand(key: string): Command =
  case key.normalize:
  of "m":
    SingleModule
  of "c":
    FullProject
  of "check":
    CheckProject
  else:
    quit "command expected"

type
  CmdMode = enum
    FromCmdLine, FromArgsFile
  CmdOptions = object
    args: seq[string]
    cmd: Command
    forceRebuild: bool
    fullRebuild: bool
    silentMake: bool
    doRun: bool
    isChild: bool
    forwardArgsToExecutable: bool
    moduleFlags: set[ModuleFlag]
    checkModes: set[CheckMode]
    config: NifConfig
    commandLineArgs: string
    commandLineArgsNifc: string
    passC: string
    passL: string
    executableArgs: string

proc createCmdOptions(baseDir: sink string): CmdOptions =
  CmdOptions(
    args: @[],
    cmd: Command.None,
    forceRebuild: false,
    fullRebuild: false,
    silentMake: false,
    doRun: false,
    moduleFlags: {},
    config: initNifConfig(baseDir),
    commandLineArgs: "",
    commandLineArgsNifc: "",
    isChild: false,
    passC: "",
    passL: "",
    checkModes: DefaultSettings,
    forwardArgsToExecutable: false,
    executableArgs: ""
  )

proc handleCmdLine(c: var CmdOptions; cmdLineArgs: seq[string]; mode: CmdMode) =
  for kind, key, val in getopt(cmdLineArgs):
    case kind
    of cmdArgument:
      if c.cmd == None:
        c.cmd = dispatchBasicCommand(key)
      else:
        if c.forwardArgsToExecutable:
          c.executableArgs.add " " & quoteShell(key)
        else:
          c.args.add key
          if c.cmd == FullProject and c.doRun and c.args.len >= 1:
            c.forwardArgsToExecutable = true

    of cmdLongOption, cmdShortOption:
      if c.forwardArgsToExecutable:
        c.executableArgs.add " --" & key
        if val.len > 0:
          c.executableArgs.add ":" & quoteShell(val)
      else:
        var forwardArg = true
        var forwardArgNifc = false
        case normalize(key)
        of "help", "h": writeHelp()
        of "version", "v": writeVersion()
        of "forcebuild", "f": c.forceRebuild = true
        of "ff":
          c.fullRebuild = true
          c.forceRebuild = true
        of "run", "r":
          c.doRun = true
          if c.cmd == FullProject and c.args.len >= 1:
            c.forwardArgsToExecutable = true
          forwardArg = false
        of "compat": c.config.compat = true
        of "path", "p":
          if mode == FromArgsFile:
            quit "`--path` in `.args` file is forbidden. Use a `nimony.paths` file instead."
          c.config.paths.add val
        of "define", "d": c.config.defines.incl val
        of "nosystem": c.moduleFlags.incl SkipSystem
        of "issystem":
          c.moduleFlags.incl IsSystem
          forwardArg = false
        of "ismain":
          c.moduleFlags.incl IsMain
          forwardArg = false
        of "bits":
          case val
          of "64": c.config.bits = 64
          of "32": c.config.bits = 32
          of "16": c.config.bits = 16
          else: quit "invalid value for --bits"
        of "cpu":
          if not c.config.setTargetCPU(val):
            quit "unknown CPU: " & val
        of "os":
          if not c.config.setTargetOS(val):
            quit "unknown OS: " & val
        of "boundchecks":
          forwardArg = false
          case val
          of "on": c.checkModes.incl BoundCheck
          of "off": c.checkModes.excl BoundCheck
          else: quit "invalid value for --boundchecks"
        of "silentmake":
          c.silentMake = true
          forwardArg = false
        of "ischild":
          # undocumented command line option, by design
          c.isChild = true
          forwardArg = false
        of "passc":
          if c.passC.len > 0:
            c.passC.add " "
          c.passC.add val
          forwardArg = false
        of "passl":
          if c.passL.len > 0:
            c.passL.add " "
          c.passL.add val
          forwardArg = false
        of "nimcache":
          c.config.nifcachePath = val
          forwardArgNifc = true
        of "cc":
          c.config.cc = val
          c.config.ccKey = extractCCKey(val)
        of "linker":
          c.config.linker = val
        of "usages":
          # set for deps.nim:
          c.config.toTrack.mode = TrackUsages
          forwardArg = true
        of "def":
          # set for deps.nim:
          c.config.toTrack.mode = TrackDef
          forwardArg = true
        else: writeHelp()
        if forwardArg:
          c.commandLineArgs.add " --" & key
          if val.len > 0:
            c.commandLineArgs.add ":" & quoteShell(val)
        if forwardArgNifc:
          c.commandLineArgsNifc.add " --" & key
          if val.len > 0:
            c.commandLineArgsNifc.add ":" & quoteShell(val)

    of cmdEnd: assert false, "cannot happen"

proc compileProgram(c: var CmdOptions) =
  if c.config.linker.len == 0 and c.config.cc.len > 0:
    c.config.linker = c.config.cc
  if c.args.len == 0:
    quit "too few command line arguments"
  elif c.args.len > 2 - int(c.cmd in {FullProject, CheckProject}):
    quit "too many command line arguments"

  if c.checkModes != DefaultSettings:
    c.commandLineArgs.add " --flags:" & genFlags(c.checkModes)

  semos.setupPaths(c.config)

  case c.cmd
  of None:
    quit "command missing"
  of SingleModule:
    if not c.isChild:
      createDir(c.config.nifcachePath)
    processSingleModule(c.args[0].addFileExt(".nim"), c.config, c.moduleFlags,
                        c.commandLineArgs, c.forceRebuild)
  of FullProject:
    createDir(c.config.nifcachePath)
    # compile full project modules
    buildGraph c.config, c.args[0], c.forceRebuild, c.silentMake,
      c.commandLineArgs, c.commandLineArgsNifc, c.moduleFlags, (if c.doRun: DoRun else: DoCompile),
      c.passC, c.passL, c.executableArgs
  of CheckProject:
    createDir(c.config.nifcachePath)
    # check full project modules
    buildGraph c.config, c.args[0], c.forceRebuild, c.silentMake,
      c.commandLineArgs, c.commandLineArgsNifc, c.moduleFlags, DoCheck, c.passC, c.passL, c.executableArgs

when isMainModule:
  var c = createCmdOptions(determineBaseDir())

  if c.config.baseDir.len > 0:
    let argsFile = findArgs(c.config.baseDir, "nimony.args")
    var args: seq[string] = @[]
    processArgsFile argsFile, args
    if args.len > 0:
      handleCmdLine(c, args, FromArgsFile)

  handleCmdLine(c, @[], FromCmdLine)
  compileProgram(c)
