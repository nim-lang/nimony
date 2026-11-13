#
#
#            Nim's Runtime Library
#        (c) Copyright 2009 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a simple high performance `CSV`:idx:
## (`comma separated value`:idx:) parser.
##
## This is a port of Nim's `std/parsecsv` to Nimony. `CsvParser` still builds
## on `std/lexbase`, so parsing is streaming and only buffers around line
## endings. Nimony models errors as `ErrorCode` values: malformed CSV raises
## `SyntaxError` and a file that cannot be opened raises `IOError`. The parser
## keeps the first error's message; use `errorMsg <#errorMsg,CsvParser>`_ after
## catching the error for a diagnostic with filename and position.
##
## Basic usage
## ===========
##
##   ```nim
##   import std/parsecsv
##   from std/os import paramStr
##   from std/streams import newFileStream
##
##   var s = newFileStream(paramStr(1), fmRead)
##   if s == nil:
##     quit("cannot open the file" & paramStr(1))
##
##   var x = default(CsvParser)
##   open(x, s, paramStr(1))
##   while readRow(x):
##     echo "new row: "
##     for val in items(x.row):
##       echo "##", val, "##"
##   close(x)
##   ```
##
## For CSV files with a header row, the header can be read and then used as a
## reference for item access with `rowEntry <#rowEntry,CsvParser,string>`_:
##
##   ```nim
##   import std/parsecsv
##
##   # Prepare a file
##   let content = """One,Two,Three,Four
##   1,2,3,4
##   10,20,30,40
##   100,200,300,400
##   """
##   writeFile("temp.csv", content)
##
##   var p = default(CsvParser)
##   p.open("temp.csv")
##   p.readHeaderRow()
##   while p.readRow():
##     echo "new row: "
##     for col in items(p.headers):
##       echo "##", col, ":", p.rowEntry(col), "##"
##   p.close()
##   ```
##
## See also
## ========
##
## * `streams module <streams.html>`_ for using
##   `open proc <#open,CsvParser,Stream,string,char,char,char>`_
##   and other stream processing (like `close proc <streams.html#close,Stream>`_)
## * `parseopt module <parseopt.html>`_ for a command line parser
## * `parsecfg module <parsecfg.html>`_ for a configuration file parser
## * `parsexml module <parsexml.html>`_ for a XML / HTML parser
## * `parsesql module <parsesql.html>`_ for a SQL parser
## * `other parsers <lib.html#pure-libraries-parsers>`_ for other parsers

{.feature: "staticContracts".}

import std/[lexbase, streams, strutils]

type
  CsvErrorKind = enum
    errNone, errQuoteExpected, errSeparatorExpected, errColumnsExpected,
    errCannotOpen, errRowChanged, errCannotGrowRow

  CsvRow* = seq[string] ## A row in a CSV file.
  CsvParser* = object of BaseLexer ## The parser object.
                                   ##
                                   ## It consists of two public fields:
                                   ## * `row` is the current row
                                   ## * `headers` are the columns that are defined in the csv file
                                   ##   (read using `readHeaderRow <#readHeaderRow,CsvParser>`_).
                                   ##   Used with `rowEntry <#rowEntry,CsvParser,string>`_).
    row*: CsvRow
    filename: string
    sep, quote, esc: char
    skipWhite: bool
    currRow: int
    headers*: seq[string]
    err: CsvErrorKind
    errLine, errCol: int
    errExpected, errFound: int

func formatError(self: CsvParser; err: CsvErrorKind;
                 line, col: int): string =
  var msg = ""
  case err
  of errNone: return ""
  of errCannotOpen: return "Error: cannot open: " & self.filename
  of errRowChanged: return "Error: row changed during parsing"
  of errCannotGrowRow: return "Error: cannot grow row"
  of errQuoteExpected: msg = $self.quote & " expected"
  of errSeparatorExpected: msg = $self.sep & " expected"
  of errColumnsExpected:
    msg = $self.errExpected & " columns expected, but found " &
          $self.errFound & " columns"
  result = "Error: "
  if self.filename.len > 0:
    result = self.filename & "(" & $line & ", " & $col & ") Error: "
  result.add msg

func hasError*(self: CsvParser): bool {.inline.} =
  ## True once the parser has recorded an error. The error message is
  ## available through `errorMsg <#errorMsg,CsvParser>`_.
  self.err != errNone

func errorMsg*(self: CsvParser): string =
  ## Returns the first error's message, including the filename and source
  ## position when available. Returns `""` if no error was recorded.
  formatError(self, self.err, self.errLine, self.errCol)

func setError(self: var CsvParser; err: CsvErrorKind; pos: int) =
  ## Keeps the first error, like a parser error state. Does not raise; callers
  ## pair this with the `ErrorCode` that describes the failure.
  if self.err == errNone:
    self.err = err
    self.errLine = self.lineNumber
    self.errCol = getColNumber(self, pos)

func fail(self: var CsvParser; err: CsvErrorKind; pos: int;
          code: ErrorCode) {.noreturn, raises.} =
  setError(self, err, pos)
  raise code

func failColumns(self: var CsvParser; line, col, expected, found: int) {.
    noreturn, raises.} =
  if self.err == errNone:
    self.err = errColumnsExpected
    self.errLine = line
    self.errCol = col
    self.errExpected = expected
    self.errFound = found
  raise SyntaxError

proc open*(self: var CsvParser; input: Stream; filename: string;
           separator = ','; quote = '"'; escape = '\0';
           skipInitialSpace = false) =
  ## Initializes the parser with an input stream. `filename` is used for error
  ## messages. The parser's behaviour can be controlled by the diverse optional
  ## parameters:
  ## - `separator`: character used to separate fields
  ## - `quote`: Used to quote fields containing special characters like
  ##   `separator`, `quote` or new-line characters. '\\0' disables the parsing
  ##   of quotes.
  ## - `escape`: removes any special meaning from the following character;
  ##   '\\0' disables escaping; if escaping is disabled and `quote` is not '\\0',
  ##   two `quote` characters are parsed one literal `quote` character.
  ## - `skipInitialSpace`: If true, whitespace immediately following the
  ##   `separator` is ignored.
  ##
  ## See also:
  ## * `open proc <#open,CsvParser,string,char,char,char>`_ which creates the
  ##   file stream for you
  runnableExamples:
    import std/streams
    var strm = newStringStream("One,Two,Three\n1,2,3\n10,20,30")
    var parser = default(CsvParser)
    parser.open(strm, "tmp.csv")
    parser.close()
    strm.close()

  lexbase.open(self, input)
  self.row.setLen(0)
  self.headers.setLen(0)
  self.currRow = 0
  self.filename = filename
  self.err = errNone
  self.errLine = 0
  self.errCol = 0
  self.errExpected = 0
  self.errFound = 0
  self.sep = separator
  self.quote = quote
  self.esc = escape
  self.skipWhite = skipInitialSpace

proc open*(self: var CsvParser; filename: string;
           separator = ','; quote = '"'; escape = '\0';
           skipInitialSpace = false) {.raises.} =
  ## Similar to the `other open proc<#open,CsvParser,Stream,string,char,char,char>`_,
  ## but creates the file stream for you. Raises `IOError` if the file cannot
  ## be opened.
  runnableExamples:
    from std/os import removeFile
    writeFile("tmp.csv", "One,Two,Three\n1,2,3\n10,20,300")
    var parser = default(CsvParser)
    parser.open("tmp.csv")
    parser.close()
    removeFile("tmp.csv")

  self.filename = filename
  self.err = errNone
  var s = newFileStream(filename, fmRead)
  if s == nil:
    self.err = errCannotOpen
    raise IOError
  open(self, s, filename, separator,
       quote, escape, skipInitialSpace)

proc parseField(self: var CsvParser; col: int) {.
    raises, requires: col >= 0 and col < self.row.len.} =
  ## Parses the field starting at `self.bufpos` into `self.row[col]`.
  ## Takes the column index instead of a `var string` so the parser and its
  ## row are not passed as aliasing mutable arguments.
  var field = move(self.row[col])
  var pos = self.bufpos
  if self.skipWhite:
    while charAt(self.buf, pos) in {' ', '\t'}: inc(pos)
  setLen(field, 0) # reuse memory
  if charAt(self.buf, pos) == self.quote and self.quote != '\0':
    inc(pos)
    while true:
      let c = charAt(self.buf, pos)
      if c == '\0':
        self.bufpos = pos
        self.fail(errQuoteExpected, pos, SyntaxError)
      elif c == self.quote:
        if self.esc == '\0' and charAt(self.buf, pos + 1) == self.quote:
          add(field, self.quote)
          inc(pos, 2)
        else:
          inc(pos)
          break
      elif c == self.esc:
        add(field, charAt(self.buf, pos + 1))
        inc(pos, 2)
      else:
        case c
        of '\c':
          pos = handleCR(self, pos)
          add(field, "\n")
        of '\l':
          pos = handleLF(self, pos)
          add(field, "\n")
        else:
          add(field, c)
          inc(pos)
  else:
    while true:
      let c = charAt(self.buf, pos)
      if c == self.sep: break
      if c in {'\c', '\l', '\0'}: break
      add(field, c)
      inc(pos)
  # Re-establish the bound after handleCR/handleLF: they take the parser by
  # `var`, so the prover no longer knows what `self.row.len` is.
  if col < 0 or col >= self.row.len:
    self.fail(errRowChanged, self.bufpos, BugError)
  self.row[col] = move(field)
  self.bufpos = pos

func processedRows*(self: var CsvParser): int {.inline.} =
  ## Returns number of the processed rows.
  ##
  ## But even if `readRow <#readRow,CsvParser,int>`_ arrived at EOF then
  ## processed rows counter is incremented.
  runnableExamples:
    import std/streams

    var strm = newStringStream("One,Two,Three\n1,2,3")
    var parser = default(CsvParser)
    parser.open(strm, "tmp.csv")
    doAssert parser.readRow()
    doAssert parser.processedRows() == 1
    doAssert parser.readRow()
    doAssert parser.processedRows() == 2
    ## Even if `readRow` arrived at EOF then `processedRows` is incremented.
    doAssert parser.readRow() == false
    doAssert parser.processedRows() == 3
    doAssert parser.readRow() == false
    doAssert parser.processedRows() == 4
    parser.close()
    strm.close()

  self.currRow

proc readRow*(self: var CsvParser; columns = 0): bool {.raises.} =
  ## Reads the next row; if `columns` > 0, it expects the row to have
  ## exactly this many columns. Returns false if the end of the file
  ## has been encountered else true.
  ##
  ## Blank lines are skipped.
  runnableExamples:
    import std/streams
    var strm = newStringStream("One,Two,Three\n1,2,3\n\n10,20,30")
    var parser = default(CsvParser)
    parser.open(strm, "tmp.csv")
    doAssert parser.readRow()
    doAssert parser.row == @["One", "Two", "Three"]
    doAssert parser.readRow()
    doAssert parser.row == @["1", "2", "3"]
    ## Blank lines are skipped.
    doAssert parser.readRow()
    doAssert parser.row == @["10", "20", "30"]

    var emptySeq: seq[string]
    doAssert parser.readRow() == false
    doAssert parser.row == emptySeq
    doAssert parser.readRow() == false
    doAssert parser.row == emptySeq

    parser.close()
    strm.close()

  var col = 0 # current column
  # skip initial empty lines #8365
  while true:
    case charAt(self.buf, self.bufpos)
    of '\c': self.bufpos = handleCR(self, self.bufpos)
    of '\l': self.bufpos = handleLF(self, self.bufpos)
    else: break
  let rowLine = self.lineNumber
  let rowCol = getColNumber(self, self.bufpos + 1)
  while charAt(self.buf, self.bufpos) != '\0':
    if self.row.len <= col:
      setLen(self.row, col + 1)
    if col >= 0 and col < self.row.len:
      parseField(self, col)
    else:
      self.fail(errCannotGrowRow, self.bufpos, BugError)
    inc(col)
    if charAt(self.buf, self.bufpos) == self.sep:
      inc(self.bufpos)
    else:
      case charAt(self.buf, self.bufpos)
      of '\c', '\l':
        # skip empty lines:
        while true:
          case charAt(self.buf, self.bufpos)
          of '\c': self.bufpos = handleCR(self, self.bufpos)
          of '\l': self.bufpos = handleLF(self, self.bufpos)
          else: break
      of '\0': discard
      else: self.fail(errSeparatorExpected, self.bufpos, SyntaxError)
      break

  setLen(self.row, col)
  result = col > 0
  if result and col != columns and columns > 0:
    self.failColumns(rowLine, rowCol, columns, col)
  inc(self.currRow)

proc close*(self: var CsvParser) {.inline.} =
  ## Closes the parser `self` and its associated input stream.
  lexbase.close(self)

proc readHeaderRow*(self: var CsvParser) {.raises.} =
  ## Reads the first row and creates a look-up table for column numbers
  ## See also:
  ## * `rowEntry proc <#rowEntry,CsvParser,string>`_
  runnableExamples:
    import std/streams

    var strm = newStringStream("One,Two,Three\n1,2,3")
    var parser = default(CsvParser)
    parser.open(strm, "tmp.csv")

    parser.readHeaderRow()
    doAssert parser.headers == @["One", "Two", "Three"]
    doAssert parser.row == @["One", "Two", "Three"]

    doAssert parser.readRow()
    doAssert parser.headers == @["One", "Two", "Three"]
    doAssert parser.row == @["1", "2", "3"]

    parser.close()
    strm.close()

  let present = self.readRow()
  if present:
    self.headers = self.row

func rowEntry*(self: var CsvParser; entry: string): var string {.raises.} =
  ## Accesses a specified `entry` from the current row.
  ##
  ## Assumes that `readHeaderRow <#readHeaderRow,CsvParser>`_ has already been
  ## called.
  ##
  ## If specified `entry` does not exist, raises KeyError.
  runnableExamples:
    import std/streams
    var strm = newStringStream("One,Two,Three\n1,2,3\n\n10,20,30")
    var parser = default(CsvParser)
    parser.open(strm, "tmp.csv")
    ## Requires calling `readHeaderRow`.
    parser.readHeaderRow()
    doAssert parser.readRow()
    doAssert parser.rowEntry("One") == "1"
    doAssert parser.rowEntry("Two") == "2"
    doAssert parser.rowEntry("Three") == "3"
    doAssertRaises(KeyError):
      discard parser.rowEntry("NonexistentEntry")
    parser.close()
    strm.close()

  let index = self.headers.find(entry)
  if index >= 0 and index < self.row.len:
    result = self.row[index]
  else:
    raise KeyError

when not defined(testing) and isMainModule:
  import std/[os, syncio]
  var s = newFileStream(paramStr(1), fmRead)
  if s == nil: quit("cannot open the file" & paramStr(1))
  var x = default(CsvParser)
  open(x, s, paramStr(1))
  try:
    while readRow(x):
      echo "new row: "
      for val in items(x.row):
        echo "##", val, "##"
  except ErrorCode as e:
    if x.hasError(): quit x.errorMsg()
    quit "parsecsv: " & $e
  close(x)
