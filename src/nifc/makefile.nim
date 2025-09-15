import std/[os, strformat, tables, syncio]
import noptions, mangler, symparser

proc generateMakefileForFiles(s: State, files: seq[string],
      action: Action,
      makefileContent: var string,
      programBody: var string,
      objectBody: var string
    ) =
  let cc = case action
      of atC:
        "$(CC)"
      of atCpp:
        "$(CXX)"
      of atNative:
        "$(CCLINKER)"
      else:
        quit "unreachable"
  let nifcacheDir = s.config.nifcacheDir
  let destExt = ExtAction[action]
  for i in 0..<files.len:
    let moduleNames = splitModulePath(files[i]).name
    makefileContent.add " " & moduleNames & ".o"
    programBody.add " " & nifcacheDir / moduleNames & ".o"
    objectBody.add &"{moduleNames}.o:\n	{cc} $(CFLAGS) -c " &
          nifcacheDir / moduleNames & fmt"{destExt} -o " &
          nifcacheDir / moduleNames & ".o\n"

proc generateMakefile*(s: State, path: string; app: string; actionTable: ActionTable) =
  var h = open(path, fmWrite)

  let optimizeLevelFlag = getoptimizeLevelFlag(s.config)
  let (cCompiler, cppCompiler) = getCompilerConfig(s.config)
  let ccLinker = if atCpp in actionTable: cppCompiler else: cCompiler

  var makefileContent = &"""
CC={cCompiler}
CXX={cppCompiler}
CFLAGS={optimizeLevelFlag}
CCLINKER={ccLinker}

program:"""
  var programBody = ""
  var objectBody = ""

  for action in actionTable.keys:
    generateMakefileForFiles(s, actionTable[action], action,
        makefileContent, programBody, objectBody)

  makefileContent.add &"\n	{ccLinker} -o " & app & programBody & "\n\n" & objectBody

  h.write(makefileContent)
  h.close()
