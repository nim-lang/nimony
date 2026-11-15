import std/[assertions, files, os, parsecsv, paths, streams, strutils, syncio]

func rowCount(p: var CsvParser): int =
  ## `processedRows` is a `func`; this wrapper verifies it stays callable from
  ## a no-side-effect context.
  p.processedRows()

func cell(p: var CsvParser; entry: string): string {.raises.} =
  ## `rowEntry` is a `func` too; returning the value (not the `var` location)
  ## is enough for the test.
  result = p.rowEntry(entry)

proc parseOne(s: string; separator = ','; quote = '"'; escape = '\0';
              skipInitialSpace = false): seq[string] {.raises.} =
  var p = default(CsvParser)
  var strm = newStringStream(s)
  p.open(strm, "mem.csv", separator, quote, escape, skipInitialSpace)
  assert p.readRow()
  result = p.row
  p.close()

proc main() {.raises.} =
  block: # basic rows, blank lines and EOF
    var p = default(CsvParser)
    var strm = newStringStream("One,Two,Three\n1,2,3\n\n10,20,30")
    p.open(strm, "tmp.csv")
    assert p.readRow()
    assert p.row == @["One", "Two", "Three"]
    assert p.readRow()
    assert p.row == @["1", "2", "3"]
    assert p.readRow()
    assert p.row == @["10", "20", "30"]
    assert rowCount(p) == 3
    assert not p.readRow()
    assert p.row.len == 0
    assert rowCount(p) == 4
    assert not p.readRow()
    assert rowCount(p) == 5
    assert not p.hasError()
    assert p.errorMsg() == ""
    p.close()

  block: # header row and rowEntry
    var p = default(CsvParser)
    var strm = newStringStream("One,Two,Three\n1,2,3\n10,20,30")
    p.open(strm, "header.csv")
    p.readHeaderRow()
    assert p.headers == @["One", "Two", "Three"]
    assert p.row == @["One", "Two", "Three"]
    assert p.readRow()
    assert cell(p, "One") == "1"
    assert cell(p, "Three") == "3"
    var keyError = false
    try:
      discard p.rowEntry("Four")
    except ErrorCode as e:
      keyError = e == KeyError
    assert keyError
    p.close()

  block: # quoted fields, doubled quotes and embedded separators
    let row = parseOne("\"a,b\",\"c\"\"d\",e")
    assert row == @["a,b", "c\"d", "e"]

  block: # backslash escaping and custom separator/quote
    assert parseOne("\"a\\\"b\",c", escape = '\\') == @["a\"b", "c"]
    assert parseOne("a;'b;c';d", separator = ';', quote = '\'') ==
      @["a", "b;c", "d"]

  block: # quoted newlines are normalized to '\n'
    assert parseOne("\"line1\nline2\",x") == @["line1\nline2", "x"]
    assert parseOne("\"a\r\nb\",x") == @["a\nb", "x"]

  block: # a field longer than lexbase's initial buffer forces a refill
    let big = repeat('x', 9000)
    let row = parseOne("\"" & big & "\",tail")
    assert row[0] == big
    assert row[1] == "tail"

  block: # initial whitespace after a separator
    assert parseOne("a, b,\tc", skipInitialSpace = true) == @["a", "b", "c"]
    assert parseOne("a, b") == @["a", " b"]

  block: # quote parsing can be disabled
    assert parseOne("\"a,b\",c", quote = '\0') == @["\"a", "b\"", "c"]

  block: # leading empty lines and CRLF
    assert parseOne("\r\n\nA,B\r\n") == @["A", "B"]

  block: # column count validation
    var p = default(CsvParser)
    var strm = newStringStream("a,b\n")
    p.open(strm, "cols.csv")
    var err = Success
    try:
      discard p.readRow(3)
    except ErrorCode as e:
      err = e
    assert err == SyntaxError
    assert p.hasError()
    assert p.errorMsg().contains("3 columns expected, but found 2 columns")
    p.close()

  block: # unterminated quotes raise SyntaxError
    var p = default(CsvParser)
    var strm = newStringStream("\"unterminated\n")
    p.open(strm, "bad.csv")
    var err = Success
    try:
      discard p.readRow()
    except ErrorCode as e:
      err = e
    assert err == SyntaxError
    assert p.hasError()
    assert p.errorMsg().contains("\" expected")
    p.close()

  block: # a missing separator raises SyntaxError and reports it
    var p = default(CsvParser)
    var strm = newStringStream("\"a\"x,b\n")
    p.open(strm, "sep.csv")
    var err = Success
    try:
      discard p.readRow()
    except ErrorCode as e:
      err = e
    assert err == SyntaxError
    assert p.hasError()
    assert p.errorMsg().contains(", expected")
    p.close()

  block: # reopening a parser resets its state
    var p = default(CsvParser)
    var first = newStringStream("A,B\n1,2\n")
    p.open(first, "first.csv")
    p.readHeaderRow()
    assert p.readRow()
    assert p.processedRows() == 2
    p.close()

    var second = newStringStream("")
    p.open(second, "second.csv")
    assert p.processedRows() == 0
    assert p.row.len == 0
    assert p.headers.len == 0
    p.readHeaderRow()
    assert p.headers.len == 0
    p.close()

  block: # the filename overload opens and closes its own stream
    let filename = getTempDir() / "nimony_tparsecsv.csv"
    try:
      writeFile(filename, "Name,Value\nalpha,1\nbeta,2\n")
    except ErrorCode:
      quit "tparsecsv: cannot write test file"
    var p = default(CsvParser)
    p.open(filename)
    p.readHeaderRow()
    assert p.readRow()
    assert cell(p, "Name") == "alpha"
    assert cell(p, "Value") == "1"
    assert p.readRow()
    assert cell(p, "Value") == "2"
    assert not p.readRow()
    p.close()
    discard tryRemoveFile(path(filename))

  block: # a missing file raises IOError
    var p = default(CsvParser)
    let filename = getTempDir() / "nimony_tparsecsv_missing.csv"
    var err = Success
    try:
      p.open(filename)
    except ErrorCode as e:
      err = e
    assert err == IOError
    assert p.hasError()
    assert p.errorMsg() == "Error: cannot open: " & filename

try:
  main()
except:
  quit "tparsecsv: unexpected exception"
echo "ok"
