#       Nif library
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Provides utility functions for the new "args configuration system".

import std/[os, strutils, syncio]

proc extractArgsKey*(command: string): string =
  ## Extract the key from a command line option.
  ##
  ## Examples:
  ## - `/usr/bin/gcc` → `gcc`
  ## - `/usr/bin/arm-linux-gnueabihf-gcc` → `arm-linux-gnueabihf-gcc`
  ## - `/usr/bin/x86_64-w64-mingw32-gcc.exe -Wall` → `x86_64-w64-mingw32-gcc`
  ## - `/usr/local/bin/clang` → `clang`
  var i = 0
  var start = 0
  while i < command.len:
    case command[i]
    of '/', '\\': start = i + 1
    of ' ', '.': break
    else: discard
    inc i
  result = command.substr(start, i-1)

proc findArgs*(baseDir: string; argsFileName: string): string =
  result = (try: expandFilename(baseDir) except: baseDir) / argsFileName
  # This particular algorithm works both with Windows (C:\\) and Unix paths and does not
  # allocate memory in its loop (except for whatever allocations `fileExists` does).
  while true:
    #echo "looking for ", result
    if fileExists(result): return result
    # as long as there is a `/path_section/` in result, remove that section
    # and continue:
    var last = result.len-1
    while last > 0 and result[last] notin {'/', '\\'}:
      dec last
    var first = last-1
    while first >= 0 and result[first] notin {'/', '\\'}:
      dec first
    if first < 0: break
    inc first # don't delete the '/'
    # this way we don't allocate at all here:
    delete result, first..last
  return "" # not found

proc processArgsFile*(argsFile: string; args: var seq[string]) =
  if argsFile.len == 0: return
  for line in lines(argsFile):
    if line.startsWith("#"):
      discard "ignore comment"
    else:
      for arg in line.splitWhitespace():
        args.add(arg)

proc processPathsFile*(pathsFile: string; paths: var seq[string]) =
  if pathsFile.len == 0: return
  for line in lines(pathsFile):
    if line.startsWith("#"):
      discard "ignore comment"
    else:
      paths.add(line)

when isMainModule:
  import std/assertions
  assert extractArgsKey("gcc") == "gcc"
  assert extractArgsKey("/usr/bin/gcc") == "gcc"
  assert extractArgsKey("/usr/bin/arm-linux-gnueabihf-gcc") == "arm-linux-gnueabihf-gcc"
  assert extractArgsKey("/usr/bin/x86_64-w64-mingw32-gcc.exe -Wall") == "x86_64-w64-mingw32-gcc"
  assert extractArgsKey("/usr/local/bin/clang") == "clang"
  assert extractArgsKey("/usr/bin/x86_64-w64-mingw32-gcc.exe -Wall") == "x86_64-w64-mingw32-gcc"
  assert extractArgsKey("/usr/local/bin/clang") == "clang"

  echo findArgs(".", "gcc.args")

  echo findArgs("C:/some/path/here/", "gcc.args")

  echo findArgs("////some/path/here/", "gcc.args")
