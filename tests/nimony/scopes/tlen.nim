
import std / syncio

const
  DirSep* = '/'
  AltSep* = when defined(windows): '\\' else: '/'

type
  Path* = distinct string

proc len*(p: Path): int {.inline.} = string(p).len
template `$`*(p: Path): string = string(p)

proc searchExtPos(path: string; len: int = 1): int =
  # Do not search until 0! .DS_Store is no file extension!
  result = -1
  for i in countdown(len(path)-1, len):
    if path[i] == '.':
      if i-1 >= 0 and path[i-1] in {DirSep, AltSep}:
        discard "bad luck"
      else:
        result = i
      break
    elif path[i] in {DirSep, AltSep}:
      break # do not skip over path

type
  Stringlike = concept
    proc len(s: Self): int
    proc `[]`(s: Self; i: int): char

proc searchExtPosG[T: Stringlike](path: T; len: int = 1): int =
  # Do not search until 0! .DS_Store is no file extension!
  result = -1
  for i in countdown(len(path)-1, len):
    if path[i] == '.':
      if i-1 >= 0 and path[i-1] in {DirSep, AltSep}:
        discard "bad luck"
      else:
        result = i
      break
    elif path[i] in {DirSep, AltSep}:
      break # do not skip over path

echo searchExtPos("abc.def"), " ", searchExtPosG("abc.def")
