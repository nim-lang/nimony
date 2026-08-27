## Driving `nimony` over a test file: the command line a category implies, and
## where the artifacts of a compile land in `nimcache/`.

import std / [syncio, os, osproc, strutils]
import ".." / lib / argsfinder
import ".." / gear2 / modnames
import context, category, markers

proc execNimony*(cmd: string; cat: Category): (string, int) =
  let cacheArg =
    if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
    else: ""
  result = execLocal("nimony", toCommand(cat) & " " & cacheArg & cmd)

proc execNimonyNative*(cmd: string): (string, int) =
  ## Compile with the C-FREE NATIVE backend (`nimony n` → arkham emits typed asm-NIF,
  ## nifasm links a static libc-free executable). Mirrors `execNimony` but selects the
  ## `n` command instead of `c`/`m`.
  let cacheArg =
    if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
    else: ""
  result = execLocal("nimony", "n --silentMake --isMain " & cacheArg & cmd)

proc pathsForFile(file: string): seq[string] =
  result = @[]
  let baseDir = file.splitFile.dir
  if baseDir.len > 0:
    let pathsFile = findArgs(baseDir, "nimony.paths")
    if pathsFile.len > 0:
      processPathsFile pathsFile, result

proc generatedFile*(orig, ext: string): string =
  let name = modnames.moduleSuffix(orig, pathsForFile(orig))
  # Backend (DCE and after) is in nimcache/<mainmod>/, see deps.nim; .s.nif is shared
  result = if ext == ".s.nif": nimcacheDir / name.addFileExt(ext)
           else: nimcacheDir / name / name.addFileExt(ext)

proc generatedExeFile*(orig: string): string =
  let name = modnames.moduleSuffix(orig, pathsForFile(orig))
  result = nimcacheDir / name / orig.splitFile.name.addFileExt(ExeExt)

proc removeMakeErrors*(output: string): string =
  result = output.strip
  for prefix in ["FAILURE:", "make:", "nifmake:"]:
    let lastLine = rfind(result, '\n')
    if lastLine >= 0:
      if result.continuesWith(prefix, lastLine+1):
        result.setLen lastLine
    elif result.startsWith(prefix):
      result.setLen 0

proc nimonyCmdFor*(file: string; cat: Category; forward: string): string =
  ## The `nimony` command line a test file is compiled with, minus the file
  ## itself. Shared by the per-file runner and the joined-group runner so a
  ## test's module is built with the exact same flags either way — they land
  ## in the same `nimcache/`, and differing flags would thrash it.
  result = "--isMain"
  case cat
  of Normal, Valgrind, Optimized, Skip: discard
  of Basics:
    result.add " --noSystem"
  of Tracked:
    result.add markersToCmdLine(extractMarkers(readFile(file)), file)
  of Compat:
    result.add " --compat"
  if forward.len != 0:
    result.add ' '
    result.add forward
  # The libc-free stdlib (native allocator + raw-syscall IO) is the compiler's
  # default now, but some tests assume the libc build, so they opt back in with
  # `-d:useLibc`: valgrind can only track the libc (mimalloc) heap — the native
  # mmap heap has no malloc hooks and is invisible to it — and the checked-in
  # golden `.nim.c` files were generated for the libc configuration.
  if cat == Valgrind or
     file.changeFileExt(".valgrind").fileExists() or
     file.changeFileExt(".nim.c").fileExists():
    result.add " -d:useLibc"
  when defined(linux):
    # Only request valgrind-tracked mimalloc when valgrind is actually present;
    # the flag pulls in `<valgrind/valgrind.h>`, which a valgrind-less box lacks.
    if hasValgrind:
      result.add " --passC:\"-DMI_TRACK_VALGRIND=1\" "
    else:
      result.add " "
  else:
    result.add " "

proc nimonyNativeCmdFor*(forward: string): string =
  ## The flags a NATIVE tree-walk compile takes, minus the file. Deliberately
  ## only `--forward`'s: `execNimonyNative` already supplies
  ## `n --silentMake --isMain`, and every flag `nimonyCmdFor` adds beyond that
  ## names something the native path does not have (`-d:useLibc` picks the libc
  ## stdlib for a golden `.nim.c` or a valgrind run, `--passC` a C compiler) —
  ## which is also why `walkUsesNative` excludes the tests that need them.
  result = ""
  if forward.len != 0:
    result.add forward
    result.add ' '
