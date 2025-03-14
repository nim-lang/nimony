#
#
#           NIFC Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## NIFC driver program.

import std / [parseopt, strutils, os, osproc, tables, assertions, syncio]
import codegen, noptions, mangler

when defined(windows):
  import bat
else:
  import makefile

when defined(enableAsm):
  import amd64 / genasm

when defined(enableLLVM):
  import llvm

const
  Version = "0.2"
  Usage = "NIFC Compiler. Version " & Version & """

  (c) 2024 Andreas Rumpf
Usage:
  nifc [options] [command] [arguments]
Command:
  c|cpp|n|llvm file.nif [file2.nif]    convert NIF files to C|C++|ASM|LLVM IR

Options:
  -r, --run                 run the makefile and the compiled program
  --compileOnly             compile only, do not run the makefile and the compiled program
  --isMain                  mark the file as the main program
  --cc:SYMBOL               specify the C compiler
  --opt:none|speed|size     optimize not at all or for speed|size
  --lineDir:on|off          generation of #line directive on|off
  --bits:N                  `(i -1)` has N bits; possible values: 64, 32, 16
  --nimcache:PATH           set the path used for generated files
  --version                 show the version
  --help                    show this help
"""

proc writeHelp() = quit(Usage, QuitSuccess)
proc writeVersion() = quit(Version & "\n", QuitSuccess)

proc genMakeCmd(config: ConfigRef, makefilePath: string): string =
  when defined(windows):
    result = "cd " & quoteShell(config.nifcacheDir) & " && " & makefilePath
  else:
    result = "cd " & quoteShell(config.nifcacheDir) & " && make -f " & makefilePath

proc generateBackend(s: var State; action: Action; filenames: seq[string];
                     flags: set[GenFlag]) =
  for inp in items filenames:
    var ext = ExtAction[action]
    let outp = s.config.nifcacheDir / changeFileExt(inp, ext)
    if action == atC:
      s.config.backend = backendC
      codegen.generateCode(s, inp, outp, flags)
    elif action == atCpp:
      s.config.backend = backendCpp
      codegen.generateCode(s, inp, outp, flags)
    elif action == atLLVM:
      when defined(enableLLVM):
        s.config.backend = backendLLVM
        # Call LLVM backend
        generateLLVMCode(s, inp, outp, flags)
      else:
        quit "nifc was not compiled with LLVM support"
    else:
      quit "target must be 'c', 'cpp', 'llvm' or 'n'"

proc handleCmdLine =
  var
    s = State(
      config: ConfigRef(cCompiler: ccGcc,
                       optimizeLevel: Speed,
                       options: {optLineDir},
                       nifcacheDir: getTempDir() / "nimcache"),
      bits: 64)
    isMain = false
    toRun = false
    compileOnly = false
    currentAction: Action = atNone
    actionTable = initActionTable()
  var p = initOptParser()
  while true:
    next(p)
    var kind = p.kind
    var key = p.key
    var val = p.val
    case kind
    of cmdArgument:
      case key.normalize
      of "c":
        currentAction = atC
        actionTable[atC] = @[]
      of "cpp":
        currentAction = atCpp
        actionTable[atCpp] = @[]
      of "n":
        currentAction = atNative
        actionTable[atNative] = @[]
      of "llvm":
        currentAction = atLLVM
        actionTable[atLLVM] = @[]
      else:
        case currentAction
        of atC:
          actionTable[atC].add key
        of atCpp:
          actionTable[atCpp].add key
        of atNative:
          actionTable[atNative].add key
        of atLLVM:
          actionTable[atLLVM].add key
        of atNone:
          quit "invalid command: " & key
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "bits":
        case val
        of "64": s.bits = 64
        of "32": s.bits = 32
        of "16": s.bits = 16
        else: quit "invalid value for --bits"
      of "help", "h": writeHelp()
      of "version", "v": writeVersion()
      of "run", "r": toRun = true
      of "compileonly": compileOnly = true
      of "ismain": isMain = true
      of "cc":
        case val.normalize
        of "gcc":
          s.config.cCompiler = ccGcc
        of "clang":
          s.config.cCompiler = ccCLang
        else:
          quit "unknown C compiler: '$1'. Available options are: gcc, clang" % [val]
      of "opt":
        case val.normalize
        of "speed":
          s.config.optimizeLevel = Speed
        of "size":
          s.config.optimizeLevel = Size
        of "none":
          s.config.optimizeLevel = None
        else:
          quit "'none', 'speed' or 'size' expected, but '$1' found" % val
      of "linedir":
        case val.normalize
        of "", "on":
          s.config.options.incl optLineDir
        of "off":
          s.config.options.excl optLineDir
        else:
          quit "'on', 'off' expected, but '$1' found" % val
      of "nimcache":
        s.config.nifcacheDir = val
      of "out", "o":
        s.config.outputFile = val
      else: writeHelp()
    of cmdEnd: assert false, "cannot happen"

  createDir(s.config.nifcacheDir)
  if actionTable.len != 0:
    for action in actionTable.keys:
      case action
      of atC, atCpp, atLLVM:
        let isLast = (if compileOnly: isMain else: currentAction == action)
        var flags = if isLast: {gfMainModule} else: {}
        if isMain:
          flags.incl gfProducesMainProc
        generateBackend(s, action, actionTable[action], flags)
      of atNative:
        let args = actionTable[action]
        if args.len == 0:
          quit "command takes a filename"
        else:
          when defined(enableAsm):
            for inp in items args:
              let outp = changeFileExt(inp, ".S")
              generateAsm inp, s.config.nifcacheDir / outp
          else:
            quit "wasn't built with native target support"
      of atNone:
        quit "targets are not specified"

    if s.selects.len > 0:
      var h = open(s.config.nifcacheDir / "select_any.h", fmWrite)
      for x in s.selects:
        write h, "#include \"" & extractFilename(x) & "\"\n"
      h.close()
    let appName = actionTable[currentAction][^1].splitFile.name.mangleFileName
    if s.config.outputFile == "":
      s.config.outputFile = appName

    if not compileOnly:
      when defined(windows):
        let makefilePath = s.config.nifcacheDir / "Makefile." & appName & ".bat"
        generateBatMakefile(s, makefilePath, s.config.outputFile, actionTable)
      else:
        let makefilePath = s.config.nifcacheDir / "Makefile." & appName
        generateMakefile(s, makefilePath, s.config.outputFile, actionTable)
      if toRun:
        let makeCmd = genMakeCmd(s.config, makefilePath)
        let (output, exitCode) = execCmdEx(makeCmd)
        if exitCode != 0:
          quit "execution of an external program failed: " & output
        if execCmd("./" & appName) != 0:
          quit "execution of an external program failed: " & appName
  else:
    writeHelp()

when isMainModule:
  handleCmdLine()
