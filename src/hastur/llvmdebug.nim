## The `tests/llvmdebug` golden suite: what the LLVM backend emits as DWARF
## debug metadata.
##
## Its own module for the same reason `native` is one: a golden suite over a
## backend's output is a concern of its own, and this one is opt-in
## (`hastur.mode = skip`) because the LLVM backend cannot build the full
## stdlib yet. A test with an `.output` sidecar is a run test instead: it
## must build with `nimony l` and print `.output`; a subdirectory's
## `nimony.args` sets its flags.

import std / [syncio, os, osproc, strutils, times, algorithm]
import context, counters, compile, builders

proc normalizeDebugLine(line: string): string =
  ## `directory: "<abs path>"` -> `directory: "<dir>"` and
  ## `X60Qini_0_<modulehash>` -> `X60Qini_0_<mod>`.
  result = line
  let dirPos = result.find("directory: \"")
  if dirPos >= 0:
    let valStart = dirPos + "directory: \"".len
    let valEnd = result.find('"', valStart)
    if valEnd > valStart:
      result = result[0 ..< valStart] & "<dir>" & result[valEnd .. ^1]
  let sufPos = result.find("X60Qini_0_")
  if sufPos >= 0:
    let valStart = sufPos + "X60Qini_0_".len
    var valEnd = valStart
    while valEnd < result.len and result[valEnd] notin {'"'}: inc valEnd
    result = result[0 ..< valStart] & "<mod>" & result[valEnd .. ^1]

proc extractDebugMetadata(llText: string): string =
  ## Keep only the debug metadata lines of a `.ll` file, in order. Everything
  ## else (instructions, types, mangled temporaries) churns with unrelated
  ## codegen changes and would make the golden useless as a debug-info guard.
  ##
  ## Two things are normalized so the golden is portable across checkouts:
  ## `directory:` holds an absolute path, and module-init/`main` symbols carry
  ## the module-hash suffix, which depends on the absolute source path.
  result = ""
  for line in llText.splitLines:
    let s = line.strip
    if s.startsWith("!") and ("DILocation" in s or "DISubprogram" in s or
                              "DILocalVariable" in s or "DIFile" in s):
      result.add normalizeDebugLine(s)
      result.add "\n"

proc runOutputTest(c: var TestCounters; file, output, cacheArg: string) =
  ## A run test: built with its directory's `nimony.args`, run, and its stdout
  ## compared with the `.output` sidecar.
  let (compilerOutput, exitCode) = execLocal("nimony", "l --silentMake " &
    cacheArg & quoteShell(file))
  if exitCode != 0:
    failure c, file, "llvm compiler exitcode 0", compilerOutput
  else:
    let (runOutput, _) = execCmdEx(quoteShell(file.generatedExeFile(BackendDirLLVM)))
    if readFile(output).strip != runOutput.strip:
      failure c, file, readFile(output), runOutput

proc runGoldenTest(c: var TestCounters; file, cacheArg: string; overwrite: bool) =
  ## A debug-info golden: the debug metadata of the emitted `.ll` is compared
  ## with `<test>.ll.expected`.
  # The LLVM backend cannot link the full stdlib yet, so the exit code is not
  # meaningful here; the `.ll` is written before linking and is what we check.
  discard execLocal("nimony", "l --silentMake --isMain --inlineframes:on " &
    cacheArg & quoteShell(file))
  let llFile = generatedFile(file, ".ll", BackendDirLLVM)
  if not llFile.fileExists():
    failure c, file, "lengc .ll", "missing: " & llFile
  else:
    let actual = extractDebugMetadata(readFile(llFile))
    let expectedFile = file.changeFileExt(".ll.expected")
    if overwrite:
      writeFile(expectedFile, actual)
    elif not expectedFile.fileExists():
      failure c, file, "golden debug metadata", "missing: " & expectedFile
    else:
      let expected = readFile(expectedFile)
      if expected.strip != actual.strip:
        failure c, file, expected, actual

proc runLLVMDebugTests*(dir: string; overwrite: bool) =
  ## Golden suite over the LLVM backend's *debug metadata*. For each `.nim`,
  ## compile with `nimony l` and diff the DWARF-relevant metadata lines of the
  ## emitted `.ll` against a checked-in `<test>.ll.expected`.
  ##
  ## Only `DIFile`/`DISubprogram`/`DILocation`/`DILocalVariable` lines are
  ## compared: those carry the inlined-frame structure (#1987) and are stable,
  ## whereas the surrounding IR churns with every unrelated codegen change.
  ##
  ## `hastur.mode = skip` - the LLVM backend cannot build the full stdlib yet,
  ## so these are opt-in via `hastur tests/llvmdebug`. Add `--overwrite` to
  ## regenerate the goldens after an intended debug-info change.
  if not skipBuild:
    buildNimonyToolchain()
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  var files: seq[string] = @[]
  for x in walkDirRec(dir):
    if x.endsWith(".nim") and
       x.extractFilename != "setup.nim":   # the runner itself, not a test
      files.add x
  sort files
  for file in files:
    inc c.total
    let cacheArg =
      if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
      else: ""
    let output = file.changeFileExt(".output")
    if output.fileExists():
      runOutputTest(c, file, output, cacheArg)
    else:
      runGoldenTest(c, file, cacheArg, overwrite)
  echo c.total - c.failures, " / ", c.total,
    " llvm-debug tests successful in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some llvm-debug tests failed."
  else:
    echo "SUCCESS."
