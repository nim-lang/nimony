import std/[syncio]

const
  DirSep* = '/'
  AltSep* = when defined(windows): '\\' else: '/'

type
  Path* = distinct string

proc len*(p: Path): int {.inline.} = string(p).len
template `$`*(p: Path): string = string(p)

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

debug splitFile(Path "foo.nim/bar.nim")
