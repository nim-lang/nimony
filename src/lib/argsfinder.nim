#       Nif library
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Provides utility functions for the new "args configuration system".

import std/[os, strutils, syncio, parseopt]

proc parseCmdArg(c: string; i: int; a: var string): int =
  # eat all delimiting whitespace
  var i = i
  while i < c.len and c[i] in {' ', '\t', '\l', '\r'}: inc(i)
  if i >= c.len: return i
  case c[i]
  of '\'', '\"':
    let delim = c[i]
    inc(i) # skip ' or "
    while i < c.len and c[i] != delim:
      add a, c[i]
      inc(i)
    if i < c.len: inc(i)
  else:
    while i < c.len and c[i] > ' ':
      add(a, c[i])
      inc(i)
  result = i

proc parseCmdLinePosix(c: string; result: var seq[string]) =
  ## Components are separated by whitespace unless the whitespace
  ## occurs within ``"`` or ``'`` quotes.
  var i = 0
  var a = ""
  while i < c.len:
    i = parseCmdArg(c, i, a)
    result.add(move a)

proc extractArgsKey*(command: string): string =
  ## Extract the key from a command line option.
  ##
  ## Examples:
  ## - `/usr/bin/gcc` → `gcc`
  ## - `/usr/bin/arm-linux-gnueabihf-gcc` → `arm-linux-gnueabihf-gcc`
  ## - `/usr/bin/x86_64-w64-mingw32-gcc.exe -Wall` → `x86_64-w64-mingw32-gcc`
  ## - `/usr/local/bin/clang` → `clang`
  result = ""
  discard parseCmdArg(command, 0, result)
  var i = 0
  var start = 0
  while i < result.len:
    case result[i]
    of '/', '\\': start = i + 1
    of '.': break
    else: discard
    inc i
  result = result.substr(start, i-1)

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
    if line.len == 0 or line.startsWith("#"):
      discard "ignore comment"
    else:
      parseCmdLinePosix(line, args)

proc processPathsFile*(pathsFile: string; paths: var seq[string]) =
  if pathsFile.len == 0: return
  let dir = pathsFile.splitFile.dir
  let expanded = try: expandFilename(dir) except: dir
  for line in lines(pathsFile):
    if line.len == 0 or line.startsWith("#"):
      discard "ignore comment"
    elif isAbsolute(line):
      paths.add(line)
    else:
      paths.add(expanded / line)

proc determineBaseDir*(mainFileAt = 1): string =
  # `mainFileAt` is the number of command line arguments to skip before
  # the base directory can be extracted from a file name. It defaults to 1
  # because the syntax is `nimony c myproject/main.nim`.
  var cmd = mainFileAt
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if cmd == 0:
        return key.splitFile.dir
      dec cmd
    else: discard
  return ""

proc extractCCKey*(val: string): string =
  result = extractArgsKey val
  var dash = result.len-1
  while dash >= 0 and result[dash] != '-': dec dash
  if dash >= 0:
    result = result.substr(dash+1)

when isMainModule:
  import std/assertions
  assert extractArgsKey("gcc") == "gcc"
  assert extractArgsKey("/usr/bin/gcc") == "gcc"
  assert extractArgsKey("/usr/bin/arm-linux-gnueabihf-gcc") == "arm-linux-gnueabihf-gcc"
  assert extractArgsKey("/usr/bin/x86_64-w64-mingw32-gcc.exe -Wall") == "x86_64-w64-mingw32-gcc"
  assert extractArgsKey("/usr/local/bin/clang") == "clang"
  assert extractArgsKey("/usr/bin/x86_64-w64-mingw32-gcc.exe -Wall") == "x86_64-w64-mingw32-gcc"
  assert extractArgsKey("/usr/local/bin/clang") == "clang"

  assert extractArgsKey("'C:\\Program Files\\gcc\\bin\\gcc.exe' -Wall") == "gcc"

  echo findArgs(".", "gcc.args")

  echo findArgs("C:/some/path/here/", "gcc.args")

  echo findArgs("////some/path/here/", "gcc.args")
