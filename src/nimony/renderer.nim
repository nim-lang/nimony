#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import ".." / lib / [bitabs, lineinfos, nifstreams, nifcursors, filelinecache]

## Rendering of Nim code from a cursor.

proc asNimCode*(n: Cursor): string =
  var m0: PackedLineInfo = NoLineInfo
  var m1: PackedLineInfo = NoLineInfo
  var nested = 0
  var n2 = n
  var file0 = FileId 0
  while true:
    if n2.info.isValid:
      let currentFile = getFileId(pool.man, n2.info)
      if not m0.isValid:
        m0 = n2.info
        file0 = currentFile
      elif not m1.isValid and currentFile == file0:
        m1 = n2.info
    case n2.kind
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else:
      discard
    if nested == 0: break
    inc n2

  when false: #if m0.isValid:
    if file0.isValid:
      let (_, line0, col0) = unpack(pool.man, m0)
      if m1.isValid:
        let (_, line1, col1) = unpack(pool.man, m1)
        result = extract(pool.files[file0],
                        FilePosition(line: line0, col: col0),
                        FilePosition(line: line1, col: col1))
      else:
        result = extract(pool.files[file0], FilePosition(line: line0, col: col0))
    else:
      result = ""
    var visible = false
    for i in 0..<result.len:
      if result[i] > ' ':
        visible = true
        break
    if not visible:
      result = toString(n, false)
  else:
    # Fallback to the NIF representation as it is much better than nothing:
    result = toString(n, false)

proc typeToString*(n: Cursor): string =
  result = asNimCode(n)
