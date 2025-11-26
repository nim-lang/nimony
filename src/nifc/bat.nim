import std/[os, strformat, tables, syncio]
import noptions, mangler, symparser

proc generateMakefileForFiles(s: State, files: seq[string],
        action: Action, links: var string): string =
  result = ""
  let cc = case action
      of atC:
        "%c_compiler%"
      of atCpp:
        "%cpp_compiler%"
      of atNative:
        "%c_linker%"
      else:
        quit "unreachable"
  let nifcacheDir = s.config.nifcacheDir
  let destExt = ExtAction[action]
  for i in 0..<files.len:
    let moduleNames = splitModulePath(files[i]).name
    links.add " " & nifcacheDir / moduleNames & ".o"
    result.add fmt"{cc} %c_flags% -c " &
          nifcacheDir / moduleNames & fmt"{destExt} -o " &
          nifcacheDir / moduleNames & ".o\n"

proc generateBatMakefile*(s: State, path: string; app: string; actionTable: ActionTable) =
  var h = open(path, fmWrite)

  let optimizeLevelFlag = getoptimizeLevelFlag(s.config)
  let (cCompiler, cppCompiler) = getCompilerConfig(s.config)

  let ccLinker = if atCpp in actionTable: cppCompiler else: cCompiler

  var makefileContent = fmt"""
@echo off

SET "c_compiler={cCompiler}"
SET "cpp_compiler={cppCompiler}"
SET "c_linker={ccLinker}"
SET "c_flags={optimizeLevelFlag}"
"""

  var links = ""

  for action in actionTable.keys:
    makefileContent.add generateMakefileForFiles(s, actionTable[action], action, links)

  makefileContent.add "\n%c_linker% -o " & app & links & "\n"

  h.write(makefileContent)
  h.close()
