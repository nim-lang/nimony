
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

proc initOptParser*(): OptParser =
  OptParser(pos: 0, inShortState: false, idx: 1, kind: cmdEnd, key: "", val: "", current: "")

proc isKeySep(c: char): bool {.inline.} = c == ':' or c == '='

proc next*(p: var OptParser) =
  if p.inShortState and p.pos < p.current.len:
    p.kind = cmdShortOption
    p.key = $p.current[p.pos]
    inc p.pos
    if p.pos < p.current.len and p.current[p.pos].isKeySep:
      inc p.pos
      p.inShortState = false
      if p.pos == p.current.len:
        p.val = paramStr(p.idx)
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
    if p.idx > paramCount():
      p.kind = cmdEnd
      return

    p.current = paramStr(p.idx)
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
        p.val = paramStr(p.idx)
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

iterator getopt*(): (CmdLineKind, string, string) =
  var p = initOptParser()
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
