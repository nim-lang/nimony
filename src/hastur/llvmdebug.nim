## The `tests/llvmdebug` golden suite: what the LLVM backend emits as DWARF
## debug metadata.
##
## Its own module for the same reason `native` is one: a golden suite over a
## backend's output is a concern of its own, and this one is opt-in
## (`hastur.mode = skip`) because the LLVM backend cannot build the full
## stdlib yet.

import std / [syncio, os, strutils, times, algorithm]
import context, counters, compile, builders

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
    var s = line.strip
    if not s.startsWith("!"): continue
    if not ("DILocation" in s or "DISubprogram" in s or
            "DILocalVariable" in s or "DIFile" in s): continue
    # `directory: "<abs path>"` -> `directory: "<dir>"`
    let dirPos = s.find("directory: \"")
    if dirPos >= 0:
      let valStart = dirPos + "directory: \"".len
      let valEnd = s.find('"', valStart)
      if valEnd > valStart:
        s = s[0 ..< valStart] & "<dir>" & s[valEnd .. ^1]
    # `X60Qini_0_<modulehash>` -> `X60Qini_0_<mod>`
    let sufPos = s.find("X60Qini_0_")
    if sufPos >= 0:
      let valStart = sufPos + "X60Qini_0_".len
      var valEnd = valStart
      while valEnd < s.len and s[valEnd] notin {'"'}: inc valEnd
      s = s[0 ..< valStart] & "<mod>" & s[valEnd .. ^1]
    result.add s
    result.add "\n"

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
    buildLengc()
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  var files: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim") and
       x.path.extractFilename != "setup.nim":   # the runner itself, not a test
      files.add x.path
  sort files
  for file in files:
    inc c.total
    let cacheArg =
      if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
      else: ""
    # The LLVM backend cannot link the full stdlib yet, so the exit code is not
    # meaningful here; the `.ll` is written before linking and is what we check.
    discard execLocal("nimony", "l --silentMake --isMain --inlineframes:on " &
      cacheArg & quoteShell(file))
    let llFile = generatedFile(file, ".ll")
    if not llFile.fileExists():
      failure c, file, "lengc .ll", "missing: " & llFile
      continue
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
  echo c.total - c.failures, " / ", c.total,
    " llvm-debug tests successful in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some llvm-debug tests failed."
  else:
    echo "SUCCESS."
