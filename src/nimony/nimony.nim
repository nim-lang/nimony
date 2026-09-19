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

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
import std / [parseopt, sets, strutils, os, assertions, syncio, dirs, paths]
import ".." / lib / [tooldirs, argsfinder, nimversion]

import ".." / gear2 / modnames
import semmain, sem, nifconfig, semos, semdata, deps, langmodes, cli

include ".." / lib / compat2

template makeDir(p: string) =
  when defined(nimony):
    onRaiseQuit createDir(path(p))
  else:
    onRaiseQuit createDir(Path(p))

const
  Usage = "Nimony Compiler. Version " & Version & """

  (c) 2024-2025 Andreas Rumpf
Usage:
  nimony [options] [command]
Command:
  c project.nim               compile the full project via C backend
  l project.nim               compile the full project via LLVM backend
  n project.nim               compile the full project via the native backend
                              (arkham + nifasm; static, libc-free executable)
  w project.nim               compile the full project via the wasm backend
                              (ithaqua; one whole-program .wasm, no linker)
  j project.nim               compile the full project via the JS backend
                              (jorogumo; one self-contained .js, no linker)
  check project.nim           check the full project for errors; can be
                              combined with `--usages`, `--def` for
                              editor integration
  m file.nim [project.nim]    compile a single Nim module to Hexer

Options:
  -d, --define:SYMBOL       define a symbol for conditional compilation
  -d:release                build in release mode (implies --opt:speed;
                            runtime checks stay on)
  -d:danger                 build in danger mode (implies --opt:speed and
                            turns every runtime check off)
  -p, --path:PATH           add PATH to the search path
  -f, --forcebuild          force a rebuild
  --ff                      force a full build
  -r, --run                 also run the compiled program
  --compat                  turn on compatibility mode
  --isSystem                passed module is a `system.nim` module
  --isMain                  passed module is the main module of a project
  --noSystem                do not auto-import `system.nim`
  --mm:STRATEGY             select the memory management strategy; the name
                            maps to `system/<strategy>.nim` in the stdlib.
                            Possible values: atomicArc (default), arc
  --bits:N                  `int` has N bits; possible values: 64, 32, 16
  --cpu:SYMBOL              set the target processor (cross-compilation)
  --os:SYMBOL               set the target operating system (cross-compilation)
  --browser                 JS backend: emit for a browser host (same as
                            `--target:browser`; `--target:node` is the
                            default). No Node `fs`/`process` in the preamble,
                            complete stdout/stderr lines go to
                            console.log/console.error, and the export surface
                            lands on globalThis.NIF.
  --silentMake              suppresses make output
  --profile                 print nifmake timing profile of executed commands
  --report                  print machine-readable per-command invocation
                            counts on stdout (one line per nifmake call)
  --stats                   after build, print total LOC and module count
                            across the dep graph
  --layout:FILE             native backend, bare-metal targets only: the BOARD
                            description (memory regions, stack slots, heap) that
                            arkham and nifasm build the image against. See
                            nativenif's doc/layout.md
  --nimcache:PATH           set the path used for generated files
                            (default `nimcache`; in --compat mode a `nimcache`
                            entry from the `nim.cfg` hierarchy is honored)
  -o, --out:PATH            write the executable to PATH (overrides the
                            default `<nimcache>/<modhash>/<basename>.exe`).
                            Splits into directory + filename like Nim;
                            combine with --outdir if you want them set
                            independently.
  --outdir:DIR              put the executable in DIR (default = cwd).
                            Same semantics as Nim's --outdir.
  --boundchecks:on|off      turn bound checks on or off
  --usages:file,line,col    list usages of the symbol at the given position
  --def:file,line,col       list definition of the symbol at the given position
  --cc:C_COMPILER           set the C compiler; can be a path to the compiler's
                            executable or a name
  --linker:LINKER           set the linker
  --app:console|gui|lib|staticlib
                            set the application type (default: console)
  --opt:speed|size|none     C compiler optimization level
                            (default: -O1, opt:speed -> -O3, opt:size -> -Os,
                             opt:none -> -O0)
  --inlineframes:on|off     record which template an expansion came from, so a
                            debug build shows template calls as inlined frames
                            (default: off)
  --novalidate              skip running the plugin validator on plugin sources
  --verbose                 dump Final IR (and other diagnostics) on contract
                            analysis failures
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc processSingleModule(nimFile: string; config: sink NifConfig; moduleFlags: set[ModuleFlag];
                         commandLineArgs, hostCommandLineArgs: string; forceRebuild: bool) =
  let nifler = parserTool()
  let name = moduleSuffix(nimFile, config.paths)
  let src = config.nifcachePath / name & ".p.nif"
  let dest = config.nifcachePath / name & ".s.nif"
  let toforceRebuild = if forceRebuild: " -f " else: ""
  exec quoteShell(nifler) & " --portablePaths p " & toforceRebuild & quoteShell(nimFile) & " " &
    quoteShell(src)
  semcheck(@[src], @[dest], ensureMove config, moduleFlags, commandLineArgs,
           hostCommandLineArgs, true)

type
  Command = enum
    None, SingleModule, FullProject, CheckProject, SemCheckNif, DocProject

proc dispatchBasicCommand(key: string; config: var NifConfig): Command =
  case key.normalize:
  of "m":
    SingleModule
  of "c":
    FullProject
  of "l":
    config.backend = backendLLVM
    FullProject
  of "n":
    # Native backend: Leng -> arkham -> nifasm, producing a static, libc-free
    # executable. arkham emits raw syscalls and nifasm writes a static image
    # with no dynamic linker, so the standard library must be compiled in its
    # native-allocator + libc-free configuration.
    config.backend = backendNative
    config.addDefine "nimNativeAlloc"
    config.addDefine "nimNativeIo"
    FullProject
  of "w":
    # Wasm backend: Leng -> ithaqua, producing one whole-program `.wasm`
    # module (no C compiler, no linker — the JS/wasm host resolves the fixed
    # env import set). The target is implied after CLI parsing (see the
    # backendWasm block in handleCmdLine): wasm32/standalone/32 bits.
    config.backend = backendWasm
    FullProject
  of "j":
    # JS backend: Leng -> jorogumo, producing one self-contained `.js` program
    # (no C compiler, no linker, no host import object — the file runs on a
    # bare `node file.js`). The target is implied after CLI parsing, exactly
    # as for wasm: the two backends share one 32-bit freestanding model.
    config.backend = backendJs
    FullProject
  of "check":
    CheckProject
  of "s":
    SemCheckNif
  of "doc":
    DocProject
  else:
    quit "invalid command, " & key

type
  CmdMode = enum
    FromCmdLine, FromArgsFile
  CmdOptions = object
    args: seq[string]
    cmd: Command
    fullRebuild: bool
    buildFlags: set[BuildFlag]  ## ForceRebuild, SilentMake, Profile, Report
    doRun: bool
    isChild: bool
    forwardArgsToExecutable: bool
    moduleFlags: set[ModuleFlag]
    checkModes: set[CheckMode]
    config: NifConfig
    commandLineArgs: string
    commandLineArgsLengc: string
    hostCommandLineArgs: string
      ## `commandLineArgs` minus the target triple: what a compile-time-eval
      ## process (a macro plugin build) is given. See `parseCommonOption`.
    passC: string
    passL: string
    executableArgs: string

proc createCmdOptions(baseDir: sink string): CmdOptions =
  CmdOptions(
    args: @[],
    cmd: Command.None,
    fullRebuild: false,
    buildFlags: {},
    doRun: false,
    moduleFlags: {},
    config: initNifConfig(baseDir),
    commandLineArgs: "",
    commandLineArgsLengc: "",
    hostCommandLineArgs: "",
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
        c.cmd = dispatchBasicCommand(key, c.config)
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
        var forwardArgLengc = false
        var forwardArgHost = true
        # Handle special cases first, then try common parser
        let keyNorm = normalize(key)
        if keyNorm == "help":
          echo Usage
          quit(QuitSuccess)
        elif keyNorm == "path" or keyNorm == "p":
          # Special handling for --path due to FromArgsFile check
          if mode == FromArgsFile:
            quit "`--path` in `.args` file is forbidden. Use a `nimony.paths` file instead."
          c.config.paths.add val
        elif (keyNorm == "define" or keyNorm == "d") and
             (normalize(val) == "release" or normalize(val) == "danger"):
          # `-d:release` / `-d:danger`: define the symbol (so `defined(release)`
          # / `defined(danger)` work here and — via forwarding (forwardArg stays
          # true) — in nimsem and the stdlib) and apply the implied build
          # settings. Mirroring Nim, `release` only raises the optimization level
          # while keeping runtime checks; `danger` additionally turns every
          # runtime check off. Later explicit `--opt:`/`--boundchecks:` still win,
          # since options are processed left to right.
          c.config.addDefine val
          c.config.optLevel = optSpeed
          if normalize(val) == "danger":
            c.checkModes = {}
        elif parseCommonOption(key, val, c.config, c.moduleFlags, forwardArg, forwardArgLengc,
                              forwardArgHost, helpMsg = Usage, versionMsg = Version & "\n"):
          discard "handled by common CLI parser"
        else:
          # Handle nimony-specific options
          case keyNorm
          of "forcebuild", "f": c.buildFlags.incl ForceRebuild
          of "ff":
            c.fullRebuild = true
            c.buildFlags.incl ForceRebuild
          of "run", "r":
            c.doRun = true
            if c.cmd == FullProject and c.args.len >= 1:
              c.forwardArgsToExecutable = true
            forwardArg = false
          of "boundchecks":
            forwardArg = false
            case val
            of "on": c.checkModes.incl BoundCheck
            of "off": c.checkModes.excl BoundCheck
            else: quit "invalid value for --boundchecks"
          of "silentmake":
            c.buildFlags.incl SilentMake
            forwardArg = false
          of "profile":
            c.buildFlags.incl Profile
            forwardArg = false
          of "report":
            c.buildFlags.incl Report
            forwardArg = false
          of "stats":
            c.buildFlags.incl Stats
            forwardArg = false
          of "ischild":
            # undocumented command line option, by design
            c.isChild = true
            forwardArg = false
          of "native":
            # Select the libc-free native backend (arkham + nifasm) for this build,
            # without using the `n` *command*. Used by macro-plugin compilation (the
            # `s` command), where skipping the C-compiler round-trip is the biggest
            # compile-time win. Mirrors `n`'s config in dispatchBasicCommand; the
            # defines are forwarded to nimsem by compileProgram (the backendNative
            # branch), so this flag itself need not propagate.
            c.config.backend = backendNative
            c.config.addDefine "nimNativeAlloc"
            c.config.addDefine "nimNativeIo"
            forwardArg = false
          of "browser":
            # JS backend: emit for a browser host instead of node. Consumed here
            # and forwarded to jorogumo as --target:browser (see deps.nim).
            c.config.jsBrowser = true
            forwardArg = false
          of "target":
            case normalize(val)
            of "browser": c.config.jsBrowser = true
            of "node", "": c.config.jsBrowser = false
            else: quit "invalid value for --target (browser|node)"
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
          else: writeHelp()
        if forwardArg:
          c.commandLineArgs.add " --" & key
          if val.len > 0:
            # Store the raw value: it is emitted into the `.build.nif` as
            # individual StringLits which nifmake shell-quotes exactly once.
            # Pre-quoting here would double-quote (e.g. `--usages:f,3,10`'s
            # comma triggers quoteShell, then nifmake quotes again and the
            # literal quotes reach the tool). The forwarded compiler flags are
            # shell-safe unquoted, so `selfExec`'s raw splice is fine too.
            c.commandLineArgs.add ":" & val
          if forwardArgHost:
            c.hostCommandLineArgs.add " --" & key
            if val.len > 0:
              c.hostCommandLineArgs.add ":" & val
        if forwardArgLengc:
          c.commandLineArgsLengc.add " --" & key
          if val.len > 0:
            c.commandLineArgsLengc.add ":" & val

    of cmdEnd: assert false, "cannot happen"

proc compileProgram(c: var CmdOptions) =
  if c.config.backend == backendNative and c.config.appType notin {appConsole, appGui}:
    quit "the native backend supports executables only (no --app:lib/staticlib)"
  if c.config.backend == backendLLVM:
    if c.config.linker.len == 0:
      c.config.linker = "clang"
  elif c.config.linker.len == 0 and c.config.cc.len > 0:
    c.config.linker = c.config.cc
  if c.args.len == 0:
    quit "too few command line arguments, try --help"
  elif c.args.len > 2 - int(c.cmd in {FullProject, CheckProject, DocProject}):
    quit "too many command line arguments"

  if c.checkModes != DefaultSettings:
    let flags = genFlags(c.checkModes)
    # Emit a bare `--flags` when no checks are active (e.g. `-d:danger`): a
    # trailing `--flags:` would make parseopt swallow the following `m` command
    # as its value. Append `:value` only when there is one, matching how every
    # other forwarded option is built below.
    c.commandLineArgs.add (if flags.len > 0: " --flags:" & flags else: " --flags")
    c.hostCommandLineArgs.add (if flags.len > 0: " --flags:" & flags else: " --flags")
  # Forward the active check modes to the hexer code generator too (nifcgen
  # injects bound/range-check calls); without this it always used DefaultSettings.
  c.config.checkFlags = genFlags(c.checkModes)

  # The libc-free stdlib is the DEFAULT for every backend now: the native
  # allocator (`nimNativeAlloc`, a ported region allocator over mmap) and
  # raw-syscall IO (`nimNativeIo`). Opt back into the libc-backed versions with
  # `-d:useLibc` (both), `-d:useMimalloc` (allocator only) or `-d:useLibcIo` (IO
  # only). The native backend links no libc at all, so it is ALWAYS libc-free
  # regardless of those opt-outs. The `when defined(...)` gates live in nimsem,
  # which only sees defines forwarded on its command line (config.defines is the
  # cache key but not enough on its own) — so inject them the same way a user's
  # `-d:` does, and record them in config.defines so the cache key tracks them.
  if c.config.backend in {backendWasm, backendJs}:
    # The wasm and JS backends have exactly one target each, and they are the
    # same target: 32-bit, freestanding, wasm32 word size. Imply it here —
    # after CLI parsing — so `nimony w x.nim` / `nimony j x.nim` work bare.
    # Forwarded like user flags because nimsem sees only its command line
    # (appended last, so it also wins over a contradictory explicit
    # --cpu/--os).
    #
    # `--os:standalone`, NOT `embedded`. Both are freestanding and it is easy to
    # read them as the same target, but they pick different stdlib arms and only
    # one of them is this one's. `embedded` is BARE METAL: `syncio`'s arm writes
    # through ARM semihosting and `osalloc`'s takes the heap from nifasm's
    # `(heapstart)`/`(heapsize)` board-layout constants — neither exists here.
    # `standalone` falls into the raw-`write`/`read`/`open` arm instead, and
    # those are exactly the names ithaqua resolves to the wasm host's import set
    # and jorogumo implements in its JS preamble (`nim_write`/`nim_exit`,
    # `memorySize`/`memoryGrow`). The `wasm32` cpu is what selects osalloc's
    # memory-growth arm; JS borrows it for the same 32-bit pointer model, it
    # does not run wasm.
    discard c.config.setTargetCPU("wasm32")
    discard c.config.setTargetOS("standalone")
    c.config.bits = 32
    c.commandLineArgs.add " --cpu:wasm32 --os:standalone --bits:32"
  let nativeBackend = c.config.backend == backendNative
  let optOutAll = c.config.isDefined("useLibc")
  if nativeBackend or not (optOutAll or c.config.isDefined("useMimalloc")):
    c.config.addDefine "nimNativeAlloc"
    c.commandLineArgs.add " --define:nimNativeAlloc"
    c.hostCommandLineArgs.add " --define:nimNativeAlloc"
  if nativeBackend or not (optOutAll or c.config.isDefined("useLibcIo")):
    c.config.addDefine "nimNativeIo"
    c.commandLineArgs.add " --define:nimNativeIo"
    c.hostCommandLineArgs.add " --define:nimNativeIo"
  # `nimNoLibc` marks the TRULY freestanding target — the native (arkham+nifasm)
  # backend, which links no libc at all. It is a stricter condition than
  # `nimNativeIo`: the C backend uses the raw-syscall stdlib too, but libc is still
  # linked, so stdlib code can fall back to a libc symbol where the freestanding
  # implementation is impossible (`futex` — no libc symbol of that name) or merely
  # approximate (`strtod`). Only the native backend sets it.
  if nativeBackend:
    c.config.addDefine "nimNoLibc"
    c.commandLineArgs.add " --define:nimNoLibc"
    c.hostCommandLineArgs.add " --define:nimNoLibc"

  semos.setupPaths(c.config)

  case c.cmd
  of None:
    quit "command missing"
  of SingleModule:
    if not c.isChild:
      makeDir(c.config.nifcachePath)
    processSingleModule(c.args[0].addFileExt(".nim"), c.config, c.moduleFlags,
                        c.commandLineArgs, c.hostCommandLineArgs, ForceRebuild in c.buildFlags)
  of FullProject:
    makeDir(c.config.nifcachePath)
    # compile full project modules
    buildGraph c.config, c.args[0].addFileExt(".nim"), c.buildFlags,
      c.commandLineArgs, c.commandLineArgsLengc, c.moduleFlags, (if c.doRun: DoRun else: DoCompile),
      c.passC, c.passL, c.executableArgs
  of CheckProject:
    makeDir(c.config.nifcachePath)
    # check full project modules
    buildGraph c.config, c.args[0].addFileExt(".nim"), c.buildFlags,
      c.commandLineArgs, c.commandLineArgsLengc, c.moduleFlags, DoCheck, c.passC, c.passL, c.executableArgs
  of DocProject:
    makeDir(c.config.nifcachePath)
    # doc full project modules
    buildGraph c.config, c.args[0].addFileExt(".nim"), c.buildFlags,
      c.commandLineArgs, c.commandLineArgsLengc, c.moduleFlags, DoDoc, c.passC, c.passL, c.executableArgs
  of SemCheckNif:
    makeDir(c.config.nifcachePath)
    # compile full project modules
    buildGraph c.config, c.args[0], c.buildFlags,
      c.commandLineArgs, c.commandLineArgsLengc, c.moduleFlags, (if c.doRun: DoRun else: DoCompile),
      c.passC, c.passL, c.executableArgs

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
