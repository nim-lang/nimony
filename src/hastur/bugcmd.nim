## `hastur bug` and `hastur rep`: compile one file to fill `nimcache/`, and
## replay the tool command line the last failure died on.

import std / [syncio, os, strutils]
import context, builders

const
  HasturSessionFile = "hastur_session.txt"

proc extractToolCmd*(output: string): string =
  result = ""
  var i = 0
  while i < output.len:
    if output.continuesWith("nifmake: ", i):
      inc i, len("nifmake: ")
      var tool = ""
      var skip = false
      while i < output.len and output[i] != ' ':
        if output[i] in {'\'', '/'}:
          tool.setLen 0
          skip = false
        elif output[i] == '.':
          skip = true
        else:
          if not skip:
            tool.add output[i]
        inc i
      if tool.len > 0:
        result = "nim c -r src/" & tool & "/" & tool & ".nim "
        while i < output.len and output[i] != '\n':
          result.add output[i]
          inc i
        # the first `nifmake` line is of interest:
        return result
    else:
      inc i

proc loadSessionCmd*(): string =
  try:
    result = readFile(HasturSessionFile).strip
  except IOError:
    result = ""

proc saveSessionCmd*(cmd: string) =
  if cmd.len > 0:
    writeFile(HasturSessionFile, cmd)

proc bugCmd*(args: seq[string]; forward: string) =
  if not fileExists("bin/nimony".addFileExt(ExeExt)):
    buildNimsem()
    buildNimony()
    buildHexer()
  var cmd = "c"
  if forward.len != 0:
    cmd.add ' '
    cmd.add forward
  for arg in items(args):
    cmd.add ' '
    cmd.add quoteShell(arg)
  let (output, exitCode) = execLocal("nimony", cmd)
  if exitCode != 0:
    stdout.write("FAILURE " & cmd & "\n")
    if output.len > 0:
      stdout.write(output)
    let toolCmd = extractToolCmd(output)
    if toolCmd.len > 0:
      saveSessionCmd(toolCmd)
    quit 1
  if output.len > 0:
    stdout.write(output)

proc repCmd*() =
  let cmd = loadSessionCmd()
  if cmd.len == 0:
    quit "no session to repeat"
  exec cmd
