
when defined(nimony):
  import strutils

  var
    nifcArgc {.importc: "cmdCount".}: int32
    nifcArgv {.importc: "cmdLine".}: ptr UncheckedArray[cstring]

  proc paramStr*(i: int): string =
    if i < nifcArgc and i >= 0:
      result = borrowCStringUnsafe(nifcArgv[i])
    else:
      result = ""

  proc paramCount*(): int =
    result = nifcArgc-1

else:
  from os import paramStr, paramCount

type
  CmdLineKind* = enum ## The detected command line token.
    cmdEnd,           ## End of command line reached
    cmdArgument,      ## An argument such as a filename
    cmdLongOption,    ## A long option such as --option
    cmdShortOption    ## A short option such as -c

  OptParser* = object
    pos: int
    inShortState: bool
    idx: int
    kind*: CmdLineKind
    key*, val*: string
    current: string
    cmdLine: seq[string]
      ## When non-empty, `next` reads from this list instead of the OS
      ## process argv. Lets callers feed the parser tokens collected from
      ## an `.args` config file.
    useCmdLine: bool

func initOptParser*(): OptParser =
  OptParser(pos: 0, inShortState: false, idx: 1, kind: cmdEnd, key: "", val: "", current: "")

func initOptParser*(cmdLine: sink seq[string]): OptParser =
  ## Parse the explicit `cmdLine` instead of the process arguments. Useful
  ## for re-parsing options collected from a `.args` file. When `cmdLine`
  ## is empty, falls back to the process argv (matches Nim's `std/parseopt`).
  if cmdLine.len == 0:
    result = initOptParser()
  else:
    result = OptParser(pos: 0, inShortState: false, idx: 0, kind: cmdEnd, key: "", val: "",
              current: "", cmdLine: cmdLine, useCmdLine: true)

func isKeySep(c: char): bool {.inline.} = c == ':' or c == '='

proc currentArg(p: OptParser; i: int): string {.inline.} =
  if p.useCmdLine:
    if i >= 0 and i < p.cmdLine.len: result = p.cmdLine[i]
    else: result = ""
  else:
    result = paramStr(i)

proc currentCount(p: OptParser): int {.inline.} =
  if p.useCmdLine: result = p.cmdLine.len - 1
  else: result = paramCount()

proc next*(p: var OptParser) =
  if p.inShortState and p.pos < p.current.len:
    p.kind = cmdShortOption
    p.key = $p.current[p.pos]
    inc p.pos
    if p.pos < p.current.len and p.current[p.pos].isKeySep:
      inc p.pos
      p.inShortState = false
      if p.pos == p.current.len:
        p.val = currentArg(p, p.idx)
        inc p.idx
        p.pos = 0
      else:
        p.val = substr(p.current, p.pos, p.current.len-1)
    else:
      if p.pos == p.current.len:
        p.inShortState = false
      p.val = ""
  else:
    p.inShortState = false
    if p.idx > currentCount(p):
      p.kind = cmdEnd
      return

    p.current = currentArg(p, p.idx)
    inc p.idx
    if p.current.len >= 2 and p.current[0] == '-' and p.current[1] == '-':
      p.kind = cmdLongOption
      var last = 2
      p.key = ""
      var hasValue = false
      while last < p.current.len:
        if isKeySep(p.current[last]):
          hasValue = true
          inc last
          break
        else:
          p.key.add p.current[last]
          inc last
      if not hasValue:
        p.val = ""
      elif last == p.current.len:
        p.val = currentArg(p, p.idx)
        inc p.idx
      else:
        p.val = substr(p.current, last, p.current.len-1)

    elif p.current.len >= 1 and p.current[0] == '-':
      p.pos = 1
      p.inShortState = true
      next p
    else:
      p.kind = cmdArgument
      p.key = move p.current
      p.val = ""

iterator getopt*(): (CmdLineKind, string, string) {.sideEffect.} =
  var p = initOptParser()
  while true:
    next(p)
    if p.kind == cmdEnd: break
    yield (p.kind, p.key, p.val)

iterator getopt*(cmdLine: sink seq[string]): (CmdLineKind, string, string) {.sideEffect.} =
  ## Same as the no-argument `getopt`, but parses `cmdLine` instead of the
  ## process argv. Useful for re-parsing options collected from an `.args`
  ## file (e.g. `nimony.args`).
  var p = initOptParser(cmdLine)
  while true:
    next(p)
    if p.kind == cmdEnd: break
    yield (p.kind, p.key, p.val)

when isMainModule:
  proc main =
    for kind, key, val in getopt():
      echo $kind, "##", key, "##", val

  main()

  # nim c -r lib\std\parseopt.nim --foo:bar -abc: 12 -def -def:5 arg1 arg0 --x: y
  #
  # produces:
  # cmdLongOption##foo##bar
  # cmdShortOption##a##
  # cmdShortOption##b##
  # cmdShortOption##c##12
  # cmdShortOption##d##
  # cmdShortOption##e##
  # cmdShortOption##f##
  # cmdShortOption##d##
  # cmdShortOption##e##
  # cmdShortOption##f##5
  # cmdArgument##arg1##
  # cmdArgument##arg0##
  # cmdLongOption##x##y
