#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Logic for mapping a (file, line, col) key to the file's content.

import std / syncio
import std/[tables, strutils]

type
  CachedFile* = object
    content: string
    lineStarts: seq[int]

  FilePosition* = object
    line*: int
    col*: int

  FileLineCache = object
    files: Table[string, CachedFile]

var
  gFileLineCache: FileLineCache

proc loadFile*(filename: string) =
  var entry = CachedFile(content: readFile(filename), lineStarts: @[0])
  var nl = find(entry.content, '\n')
  while nl > 0:
    entry.lineStarts.add nl
    nl = find(entry.content, '\n', nl + 1)
  gFileLineCache.files[filename] = ensureMove entry

proc extract*(filename: string; start, finish: FilePosition): string =
  if not gFileLineCache.files.hasKey(filename):
    loadFile(filename)
  let entry {.cursor.} = gFileLineCache.files[filename]
  let startIdx = entry.lineStarts[start.line-1] + start.col-1
  let finishIdx = entry.lineStarts[finish.line-1] + finish.col-1
  result = entry.content[startIdx..finishIdx]

proc extract*(filename: string; start: FilePosition): string =
  if not gFileLineCache.files.hasKey(filename):
    loadFile(filename)
  let entry {.cursor.} = gFileLineCache.files[filename]
  let startIdx = entry.lineStarts[start.line-1] + start.col-1
  if start.line < entry.lineStarts.len:
    let finishIdx = entry.lineStarts[start.line]-1
    result = entry.content[startIdx..finishIdx]
  else:
    result = entry.content.substr(startIdx)

when isMainModule:
  echo extract("src/lib/filelinecache.nim", FilePosition(line: 1, col: 9), FilePosition(line: 3, col: 2))
  echo extract("src/lib/filelinecache.nim", FilePosition(line: 1, col: 9))
