#
#
#            Nim's Runtime Library
#        (c) Copyright 2010 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The `parsecfg` module implements a high performance configuration file
## parser. The configuration file's syntax is similar to the Windows `.ini`
## format, but much more powerful, as it is not a line based parser. String
## literals, raw string literals and triple quoted string literals are supported
## as in the Nim programming language.
##
## This is a port of Nim's `std/parsecfg` to Nimony. Parsing is event based and
## malformed input is reported as a `cfgError` event instead of an exception.
## Opening a file that does not exist raises `IOError`, like the other Nimony
## parsers.
##
## Example of how a configuration file may look like:
##
##     ```none
##     charset = "utf-8"
##     [Package]
##     name = "hello"
##     --threads:on
##     [Author]
##     name = "nim-lang"
##     website = "nim-lang.org"
##     ```
##
## Here is an example of how to use the configuration file parser:
##
##     ```nim
##     import std/[parsecfg, streams]
##
##     let configFile = "example.ini"
##     var f = newFileStream(configFile, fmRead)
##     assert f != nil, "cannot open " & configFile
##     var p = CfgParser()
##     open(p, f, configFile)
##     while true:
##       var e = next(p)
##       case e.kind
##       of cfgEof: break
##       of cfgSectionStart:
##         echo "new section: " & e.section
##       of cfgKeyValuePair:
##         echo "key-value-pair: " & e.key & ": " & e.value
##       of cfgOption:
##         echo "command: " & e.key & ": " & e.value
##       of cfgError:
##         echo e.msg
##     close(p)
##     ```

{.feature: "staticContracts".}

import std/[lexbase, streams, strutils, tables]

type
  CfgEventKind* = enum ## enumeration of all events that may occur when parsing
    cfgEof,            ## end of file reached
    cfgSectionStart,   ## a `[section]` has been parsed
    cfgKeyValuePair,   ## a `key=value` pair has been detected
    cfgOption,         ## a `--key=value` command line option
    cfgError           ## an error occurred during parsing

  CfgEvent* = object ## describes a parsing event
    case kind*: CfgEventKind ## the kind of the event
    of cfgEof: nil
    of cfgSectionStart:
      section*: string ## `section` contains the name of the parsed section
    of cfgKeyValuePair, cfgOption:
      key*, value*: string ## the parsed key and value
    of cfgError:
      msg*: string ## the parser error message

  TokKind = enum
    tkInvalid, tkEof,
    tkSymbol, tkEquals, tkColon, tkBracketLe, tkBracketRi, tkDashDash

  Token = object
    kind: TokKind
    literal: string

  CfgParser* = object of BaseLexer ## the parser object.
    tok: Token
    filename: string

const
  SymChars = {'a'..'z', 'A'..'Z', '0'..'9', '_', ' ', '\x80'..'\xFF', '.', '/',
              '\\', '-'}

func handleHexChar(c: char; x: var int): bool {.inline.} =
  ## Converts a hexadecimal character to its ordinal value and folds the result
  ## into `x`. Inlined here to avoid a dependency on `std/private/decode_helpers`.
  result = true
  case c
  of '0'..'9': x = (x shl 4) or (ord(c) - ord('0'))
  of 'a'..'f': x = (x shl 4) or (ord(c) - ord('a') + 10)
  of 'A'..'F': x = (x shl 4) or (ord(c) - ord('A') + 10)
  else:
    result = false

proc rawGetTok(c: var CfgParser)

proc open*(c: var CfgParser; input: Stream; filename: string;
           lineOffset = 0) =
  ## Initializes the parser with an input stream. `filename` is only used for
  ## error messages. `lineOffset` can be used to influence the line number
  ## information in the generated error messages.
  lexbase.open(c, input)
  c.filename = filename
  c.tok.kind = tkInvalid
  c.tok.literal = ""
  inc(c.lineNumber, lineOffset)
  rawGetTok(c)

proc close*(c: var CfgParser) =
  ## Closes the parser `c` and its associated input stream.
  lexbase.close(c)

func getColumn*(c: CfgParser): int =
  ## Gets the current column the parser has arrived at.
  getColNumber(c, c.bufpos)

func getLine*(c: CfgParser): int =
  ## Gets the current line the parser has arrived at.
  c.lineNumber

func getFilename*(c: CfgParser): string =
  ## Gets the filename of the file that the parser processes.
  c.filename

func handleDecChars(c: var CfgParser; xi: var int) =
  while charAt(c.buf, c.bufpos) in {'0'..'9'}:
    xi = (xi * 10) + (ord(charAt(c.buf, c.bufpos)) - ord('0'))
    inc(c.bufpos)

func getEscapedChar(c: var CfgParser) =
  inc(c.bufpos) # skip '\'
  case charAt(c.buf, c.bufpos)
  of 'n', 'N':
    add(c.tok.literal, "\n")
    inc(c.bufpos)
  of 'r', 'R', 'c', 'C':
    add(c.tok.literal, '\c')
    inc(c.bufpos)
  of 'l', 'L':
    add(c.tok.literal, '\L')
    inc(c.bufpos)
  of 'f', 'F':
    add(c.tok.literal, '\f')
    inc(c.bufpos)
  of 'e', 'E':
    add(c.tok.literal, '\e')
    inc(c.bufpos)
  of 'a', 'A':
    add(c.tok.literal, '\a')
    inc(c.bufpos)
  of 'b', 'B':
    add(c.tok.literal, '\b')
    inc(c.bufpos)
  of 'v', 'V':
    add(c.tok.literal, '\v')
    inc(c.bufpos)
  of 't', 'T':
    add(c.tok.literal, '\t')
    inc(c.bufpos)
  of '\'', '"':
    add(c.tok.literal, charAt(c.buf, c.bufpos))
    inc(c.bufpos)
  of '\\':
    add(c.tok.literal, '\\')
    inc(c.bufpos)
  of 'x', 'X':
    inc(c.bufpos)
    var xi = 0
    if handleHexChar(charAt(c.buf, c.bufpos), xi):
      inc(c.bufpos)
      if handleHexChar(charAt(c.buf, c.bufpos), xi):
        inc(c.bufpos)
    if xi >= 0 and xi <= 255: add(c.tok.literal, chr(xi))
    else: c.tok.kind = tkInvalid
  of '0'..'9':
    var xi = 0
    handleDecChars(c, xi)
    if xi >= 0 and xi <= 255: add(c.tok.literal, chr(xi))
    else: c.tok.kind = tkInvalid
  else:
    c.tok.kind = tkInvalid

proc handleCRLF(c: var CfgParser; pos: int): int =
  case charAt(c.buf, pos)
  of '\c': result = lexbase.handleCR(c, pos)
  of '\L': result = lexbase.handleLF(c, pos)
  else: result = pos

proc getString(c: var CfgParser; rawMode: bool) =
  var pos = c.bufpos + 1 # skip "
  c.tok.kind = tkSymbol
  if charAt(c.buf, pos) == '"' and charAt(c.buf, pos + 1) == '"':
    # long string literal:
    inc(pos, 2) # skip ""
    pos = handleCRLF(c, pos) # skip leading newline
    var closed = false
    while true:
      case charAt(c.buf, pos)
      of '"':
        if charAt(c.buf, pos + 1) == '"' and charAt(c.buf, pos + 2) == '"':
          closed = true
          break
        add(c.tok.literal, '"')
        inc(pos)
      of '\c', '\L':
        pos = handleCRLF(c, pos)
        add(c.tok.literal, "\n")
      of EndOfFile:
        c.tok.kind = tkInvalid
        break
      else:
        add(c.tok.literal, charAt(c.buf, pos))
        inc(pos)
    if closed:
      c.bufpos = pos + 3 # skip the three """
    else:
      c.bufpos = pos
  else:
    # ordinary string literal
    while true:
      let ch = charAt(c.buf, pos)
      if ch == '"':
        inc(pos) # skip '"'
        break
      if ch in {'\c', '\L', EndOfFile}:
        c.tok.kind = tkInvalid
        break
      if ch == '\\' and not rawMode:
        c.bufpos = pos
        getEscapedChar(c)
        pos = c.bufpos
      else:
        add(c.tok.literal, ch)
        inc(pos)
    c.bufpos = pos

func getSymbol(c: var CfgParser) =
  var pos = c.bufpos
  while true:
    add(c.tok.literal, charAt(c.buf, pos))
    inc(pos)
    if charAt(c.buf, pos) notin SymChars:
      break

  while c.tok.literal.len > 0 and c.tok.literal[^1] == ' ':
    c.tok.literal.setLen(c.tok.literal.len - 1)

  c.bufpos = pos
  c.tok.kind = tkSymbol

proc skip(c: var CfgParser) =
  var pos = c.bufpos
  while true:
    case charAt(c.buf, pos)
    of ' ', '\t':
      inc(pos)
    of '#', ';':
      while charAt(c.buf, pos) notin {'\c', '\L', EndOfFile}:
        inc(pos)
    of '\c', '\L':
      pos = handleCRLF(c, pos)
    else:
      break # EndOfFile also leaves the loop
  c.bufpos = pos

proc rawGetTok(c: var CfgParser) =
  c.tok.kind = tkInvalid
  setLen(c.tok.literal, 0)
  skip(c)
  case charAt(c.buf, c.bufpos)
  of '=':
    c.tok.kind = tkEquals
    inc(c.bufpos)
    c.tok.literal = "="
  of '-':
    inc(c.bufpos)
    if charAt(c.buf, c.bufpos) == '-':
      inc(c.bufpos)
      c.tok.kind = tkDashDash
      c.tok.literal = "--"
    else:
      dec(c.bufpos)
      getSymbol(c)
  of ':':
    c.tok.kind = tkColon
    inc(c.bufpos)
    c.tok.literal = ":"
  of 'r', 'R':
    if charAt(c.buf, c.bufpos + 1) == '"':
      inc(c.bufpos)
      getString(c, true)
    else:
      getSymbol(c)
  of '[':
    c.tok.kind = tkBracketLe
    inc(c.bufpos)
    c.tok.literal = "["
  of ']':
    c.tok.kind = tkBracketRi
    inc(c.bufpos)
    c.tok.literal = "]"
  of '"':
    getString(c, false)
  of EndOfFile:
    c.tok.kind = tkEof
    c.tok.literal = "[EOF]"
  else:
    getSymbol(c)

func errorStr*(c: CfgParser; msg: string): string =
  ## Returns a properly formatted error message containing current line and
  ## column information.
  c.filename & "(" & $getLine(c) & ", " & $getColumn(c) & ") Error: " & msg

func warningStr*(c: CfgParser; msg: string): string =
  ## Returns a properly formatted warning message containing current line and
  ## column information.
  c.filename & "(" & $getLine(c) & ", " & $getColumn(c) & ") Warning: " & msg

func ignoreMsg*(c: CfgParser; e: CfgEvent): string =
  ## Returns a properly formatted warning message containing that an entry is
  ## ignored.
  case e.kind
  of cfgSectionStart: result = c.warningStr("section ignored: " & e.section)
  of cfgKeyValuePair: result = c.warningStr("key ignored: " & e.key)
  of cfgOption:
    result = c.warningStr("command ignored: " & e.key & ": " & e.value)
  of cfgError: result = e.msg
  of cfgEof: result = ""

proc getKeyValPair(c: var CfgParser; kind: CfgEventKind): CfgEvent =
  if c.tok.kind == tkSymbol:
    case kind
    of cfgOption:
      result = CfgEvent(kind: cfgOption, key: move(c.tok.literal), value: "")
    of cfgKeyValuePair:
      result = CfgEvent(kind: cfgKeyValuePair, key: move(c.tok.literal),
                        value: "")
    else:
      result = CfgEvent(kind: cfgError, msg: errorStr(c, "internal error"))
    rawGetTok(c)
    if c.tok.kind in {tkEquals, tkColon}:
      rawGetTok(c)
      if c.tok.kind == tkSymbol:
        result.value = c.tok.literal
      else:
        result = CfgEvent(kind: cfgError,
          msg: errorStr(c, "symbol expected, but found: " & c.tok.literal))
      rawGetTok(c)
  else:
    result = CfgEvent(kind: cfgError,
      msg: errorStr(c, "symbol expected, but found: " & c.tok.literal))
    rawGetTok(c)

proc next*(c: var CfgParser): CfgEvent =
  ## Retrieves the first/next event. This controls the parser.
  case c.tok.kind
  of tkEof:
    result = CfgEvent(kind: cfgEof)
  of tkDashDash:
    rawGetTok(c)
    result = getKeyValPair(c, cfgOption)
  of tkSymbol:
    result = getKeyValPair(c, cfgKeyValuePair)
  of tkBracketLe:
    rawGetTok(c)
    if c.tok.kind == tkSymbol:
      result = CfgEvent(kind: cfgSectionStart, section: move(c.tok.literal))
    else:
      result = CfgEvent(kind: cfgError,
        msg: errorStr(c, "symbol expected, but found: " & c.tok.literal))
    rawGetTok(c)
    if c.tok.kind == tkBracketRi:
      rawGetTok(c)
    else:
      result = CfgEvent(kind: cfgError,
        msg: errorStr(c, "']' expected, but found: " & c.tok.literal))
  of tkInvalid, tkEquals, tkColon, tkBracketRi:
    result = CfgEvent(kind: cfgError,
      msg: errorStr(c, "invalid token: " & c.tok.literal))
    rawGetTok(c)

# ---------------- Configuration file related operations ----------------

type
  ConfigSection* = OrderedTable[string, string]
  Config* = ref object ## A parsed configuration file.
    data: OrderedTable[string, ConfigSection]

func newConfig*(): Config =
  ## Creates a new configuration table.
  Config(data: initOrderedTable[string, ConfigSection]())

proc loadConfig*(stream: Stream; filename: string = "[stream]"): Config {.raises.} =
  ## Loads the specified configuration from stream into a new Config instance.
  ## `filename` is only used for nicer error messages.
  result = newConfig()
  var curSection = "" ## Current section; "" is the common section.
  var p = CfgParser()
  open(p, stream, filename)
  try:
    while true:
      let e = next(p)
      case e.kind
      of cfgEof:
        break
      of cfgSectionStart:
        curSection = e.section
      of cfgKeyValuePair:
        if not result.data.hasKey(curSection):
          result.data[curSection] = initOrderedTable[string, string]()
        result.data[curSection][e.key] = e.value
      of cfgOption:
        if not result.data.hasKey(curSection):
          result.data[curSection] = initOrderedTable[string, string]()
        result.data[curSection]["--" & e.key] = e.value
      of cfgError:
        break
  finally:
    close(p)

proc loadConfig*(filename: string): Config {.raises.} =
  ## Loads the specified configuration file into a new Config instance.
  ## Raises `IOError` if the file cannot be opened.
  let stream = openFileStream(filename, fmRead)
  try:
    result = loadConfig(stream, filename)
  finally:
    stream.close()

func replace(s: string): string =
  var d = ""
  var i = 0
  while i < s.len:
    if s[i] == '\\':
      d.add(r"\\")
    elif s[i] == '\c' and i + 1 < s.len and s[i + 1] == '\l':
      d.add(r"\c\l")
      inc(i)
    elif s[i] == '\c':
      d.add(r"\n")
    elif s[i] == '\L':
      d.add(r"\n")
    else:
      d.add(s[i])
    inc(i)
  result = d

proc writeConfig*(dict: Config; stream: Stream) {.raises.} =
  ## Writes the contents of the table to the specified stream.
  ##
  ## Comment statements are ignored.
  for section, sectionData in uncheckedBorrow(dict.data).pairs:
    if section != "":
      if not allCharsInSet(section, SymChars):
        stream.writeLine("[\"" & section & "\"]")
      else:
        stream.writeLine("[" & section & "]")
    for key, value in sectionData.pairs:
      var kv = ""
      var segmentChar = ""
      if key.len > 1 and key[0] == '-' and key[1] == '-':
        segmentChar = ":"
        if not allCharsInSet(key[2..key.len() - 1], SymChars):
          kv.add("--\"")
          kv.add(key[2..key.len() - 1])
          kv.add("\"")
        else:
          kv = key
      else:
        segmentChar = "="
        kv = key
      if value != "":
        if not allCharsInSet(value, SymChars):
          if find(value, '"') == -1:
            kv.add(segmentChar)
            kv.add("\"")
            kv.add(replace(value))
            kv.add("\"")
          else:
            kv.add(segmentChar)
            kv.add("\"\"\"")
            kv.add(replace(value))
            kv.add("\"\"\"")
        else:
          kv.add(segmentChar)
          kv.add(value)
      stream.writeLine(kv)

proc `$`*(dict: Config): string {.raises.} =
  ## Writes the contents of the table to a string.
  ##
  ## Comment statements are ignored.
  let stream = newStringStream()
  try:
    dict.writeConfig(stream)
    result = stream.data
  finally:
    stream.close()

proc writeConfig*(dict: Config; filename: string) {.raises.} =
  ## Writes the contents of the table to the specified configuration file.
  ##
  ## Comment statements are ignored.
  let stream = openFileStream(filename, fmWrite)
  try:
    dict.writeConfig(stream)
  finally:
    stream.close()

func getSectionValue*(dict: Config; section, key: string;
                      defaultVal = ""): string =
  ## Gets the key value of the specified section.
  ## Returns the specified default value if the key does not exist.
  dict.data.getOrDefault(section, default(ConfigSection)).getOrDefault(
    key, defaultVal)

proc setSectionKey*(dict: var Config; section, key, value: string) =
  ## Sets the key value of the specified section.
  if not dict.data.hasKey(section):
    dict.data[section] = initOrderedTable[string, string]()
  dict.data.getOrQuit(section)[key] = value

proc delSection*(dict: var Config; section: string) =
  ## Deletes the specified section and all of its sub keys.
  dict.data.del(section)

proc delSectionKey*(dict: var Config; section, key: string) =
  ## Deletes the key of the specified section.
  if dict.data.hasKey(section):
    if dict.data.getOrQuit(section).hasKey(key):
      if dict.data.getOrQuit(section).len == 1:
        dict.data.del(section)
      else:
        dict.data.getOrQuit(section).del(key)

iterator sections*(dict: Config): lent string =
  ## Iterates through the sections in `dict`.
  for section in uncheckedBorrow(dict.data).keys:
    yield section

# Table-like access for compatibility with the upstream `Config` table ref.

func hasKey*(dict: Config; section: string): bool =
  ## True if `section` exists in `dict`.
  dict.data.hasKey(section)

func contains*(dict: Config; section: string): bool {.inline.} =
  ## Alias for `hasKey`.
  dict.hasKey(section)

func len*(dict: Config): int {.inline.} =
  ## The number of sections in `dict`.
  dict.data.len

proc `[]`*(dict: Config; section: string): var ConfigSection {.raises.} =
  ## Retrieves `section`, raising `KeyError` when it is absent.
  dict.data[section]

proc `[]=`*(dict: Config; section: string; value: sink ConfigSection) =
  ## Inserts or replaces `section`.
  dict.data[section] = value

proc del*(dict: Config; section: string) =
  ## Removes `section`; does nothing when it is absent.
  dict.data.del(section)

proc clear*(dict: Config) =
  ## Removes all sections from `dict`.
  dict.data.clear()

iterator pairs*(dict: Config): (lent string, lent ConfigSection) =
  ## Iterates over `(section, values)` pairs in insertion order.
  for section, values in uncheckedBorrow(dict.data).pairs:
    yield (section, values)

iterator keys*(dict: Config): lent string =
  ## Iterates over the section names in insertion order.
  for section in uncheckedBorrow(dict.data).keys:
    yield section

iterator values*(dict: Config): lent ConfigSection =
  ## Iterates over the section value tables in insertion order.
  for values in uncheckedBorrow(dict.data).values:
    yield values
