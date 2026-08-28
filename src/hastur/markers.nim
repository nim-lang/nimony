## `#[ ^suggest ]#` markers in a `track` test: parsing them out of the source
## and turning them back into nimony's `--track:`/`--def:`/`--usages:` flags.

type
  LineInfo = object
    line, col: int
    filename: string

proc extractMarkers*(s: string): seq[LineInfo] =
  ## Extracts markers like #[  ^suggest]# from a .nim file and translates the marker
  ## into (line, col) coordinates along with the marker's content which is 'suggest'
  ## in the example.
  var i = 0
  var line = 1
  var col = 1
  var markerAt = high(int)
  var inComment = 0
  var inLineComment = false
  result = @[]
  while i < s.len:
    case s[i]
    of '#':
      if i+1 < s.len and s[i+1] == '[':
        inc inComment
      else:
        inLineComment = true
    of ']':
      if i+1 < s.len and s[i+1] == '#':
        if inComment > 0:
          dec inComment
          markerAt = high(int)
    of '^':
      if inComment > 0 or inLineComment:
        markerAt = i
        result.add LineInfo(line: line-1, col: col, filename: "")
        #           ^ a marker refers to the previous line
    of '\n':
      inc line
      col = 0
      if inLineComment:
        inLineComment = false
        markerAt = high(int)
    of '\r':
      dec col
    else: discard
    if markerAt < i:
      result[^1].filename.add s[i]
    inc i
    inc col

proc markersToCmdLine*(s: seq[LineInfo]; file: string): string =
  result = ""
  for x in items(s):
    case x.filename
    of "usages":
      result.add " --usages:" & file & "," & $x.line & "," & $x.col
    of "def":
      result.add " --def:" & file & "," & $x.line & "," & $x.col
    else:
      result.add " --track:" & $x.line & ":" & $x.col & ":" & x.filename
