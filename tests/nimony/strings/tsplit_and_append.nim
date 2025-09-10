
import std / [syncio, strutils]

const
  DirSep* = '/'
  AltSep* = when defined(windows): '\\' else: '/'

type
  Path* = distinct string

proc len*(p: Path): int {.inline.} = string(p).len
template `$`*(p: Path): string = string(p)

proc searchExtPos(path: string): int =
  # Do not search until 0! .DS_Store is no file extension!
  result = -1
  for i in countdown(len(path)-1, 1):
    if path[i] == '.':
      if i-1 >= 0 and path[i-1] in {DirSep, AltSep}:
        discard "bad luck"
      else:
        result = i
      break
    elif path[i] in {DirSep, AltSep}:
      break # do not skip over path

type
  ParsedFilename* = object
    dir*: Path
    file*: string
    ext*: string

proc debug*(f: ParsedFilename) =
  echo f.dir.string
  echo f.file
  echo f.ext

proc splitFile(p: string): ParsedFilename =
  var extPos = p.len
  var pathEnd = -1
  for i in countdown(p.len-1, 1):
    if p[i] == '.':
      if i-1 >= 0 and p[i-1] in {DirSep, AltSep}:
        discard "bad luck"
      else:
        if extPos == p.len:
          extPos = i
    elif p[i] in {DirSep, AltSep}:
      pathEnd = i
      break
  result = ParsedFilename(
    dir: Path(substr(p, 0, pathEnd)),
    file: substr(p, pathEnd+1, extPos-1),
    ext: substr(p, extPos, p.len-1))

proc splitFile*(p: Path): ParsedFilename = splitFile(string(p))

#proc `/`*(a, b: Path): Path =
#  Path(a.string & "/" & b.string)

#let x = splitFile(Path "foo.nim\\bar.nim")
#debug x
debug splitFile(Path "foo.nim/bar.nim")
#debug splitFile(Path "barz.nim")
#debug splitFile(Path "foo.nim/")

proc appendTest =
  var s = ""
  for i in 0..<10:
    s.add "abc"
  echo s

appendTest()
