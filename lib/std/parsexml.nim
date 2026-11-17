#
#
#            Nim's Runtime Library
#        (c) Copyright 2010 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a simple high performance `XML`:idx: / `HTML`:idx:
## parser.
## The only encoding that is supported is UTF-8. The parser has been designed
## to be somewhat error correcting, so that even most "wild HTML" found on the
## web can be parsed with it. **Note:** This parser does not check that each
## `<tag>` has a corresponding `</tag>`! These checks have do be
## implemented by the client code for various reasons:
##
## * Old HTML contains tags that have no end tag: `<br>` for example.
## * HTML tags are case insensitive, XML tags are case sensitive. Since this
##   library can parse both, only the client knows which comparison is to be
##   used.
## * Thus the checks would have been very difficult to implement properly with
##   little benefit, especially since they are simple to implement in the
##   client. The client should use the `errorMsgExpected` proc to generate
##   a nice error message that fits the other error messages this library
##   creates.
##
##

##[

Example 1: Retrieve HTML title
==============================

The file `examples/htmltitle.nim` demonstrates how to use the
XML parser to accomplish a simple task: To determine the title of an HTML
document.

  ```nim
  # Example program to show the parsexml module
  # This program reads an HTML file and writes its title to stdout.
  # Errors and whitespace are ignored.

  import std/[os, streams, parsexml, strutils]

  if paramCount() < 1:
    quit("Usage: htmltitle filename[.html]")

  var filename = addFileExt(paramStr(1), "html")
  var s = newFileStream(filename, fmRead)
  if s == nil: quit("cannot open the file " & filename)
  var x: XmlParser
  open(x, s, filename)
  while true:
    x.next()
    case x.kind
    of xmlElementStart:
      if cmpIgnoreCase(x.elementName, "title") == 0:
        var title = ""
        x.next()  # skip "<title>"
        while x.kind == xmlCharData:
          title.add(x.charData)
          x.next()
        if x.kind == xmlElementEnd and cmpIgnoreCase(x.elementName, "title") == 0:
          echo("Title: " & title)
          quit(0) # Success!
        else:
          echo(x.errorMsgExpected("/title"))

    of xmlEof: break # end of file reached
    else: discard # ignore other events

  x.close()
  quit("Could not determine title!")
  ```

]##

##[

Example 2: Retrieve all HTML links
==================================

The file `examples/htmlrefs.nim` demonstrates how to use the
XML parser to accomplish another simple task: To determine all the links
an HTML document contains.

  ```nim
  # Example program to show the new parsexml module
  # This program reads an HTML file and writes all its used links to stdout.
  # Errors and whitespace are ignored.

  import std/[os, streams, parsexml, strutils]

  proc `=?=` (a, b: string): bool =
    # little trick: define our own comparator that ignores case
    return cmpIgnoreCase(a, b) == 0

  if paramCount() < 1:
    quit("Usage: htmlrefs filename[.html]")

  var links = 0 # count the number of links
  var filename = addFileExt(paramStr(1), "html")
  var s = newFileStream(filename, fmRead)
  if s == nil: quit("cannot open the file " & filename)
  var x: XmlParser
  open(x, s, filename)
  next(x) # get first event
  block mainLoop:
    while true:
      case x.kind
      of xmlElementOpen:
        # the <a href = "xyz"> tag we are interested in always has an attribute,
        # thus we search for `xmlElementOpen` and not for `xmlElementStart`
        if x.elementName =?= "a":
          x.next()
          if x.kind == xmlAttribute:
            if x.attrKey =?= "href":
              var link = x.attrValue
              inc(links)
              # skip until we have an `xmlElementClose` event
              while true:
                x.next()
                case x.kind
                of xmlEof: break mainLoop
                of xmlElementClose: break
                else: discard
              x.next() # skip `xmlElementClose`
              # now we have the description for the `a` element
              var desc = ""
              while x.kind == xmlCharData:
                desc.add(x.charData)
                x.next()
              echo(desc & ": " & link)
        else:
          x.next()
      of xmlEof: break # end of file reached
      of xmlError:
        echo(errorMsg(x))
        x.next()
      else: x.next() # skip other events

  echo($links & " link(s) found!")
  x.close()
  ```

]##

{.feature: "staticContracts".}

import std/[assertions, strutils, lexbase, streams, unicode]

# the parser treats `<br />` as `<br></br>`

#  xmlElementCloseEnd, ## `/>`

type
  XmlEventKind* = enum ## enumeration of all events that may occur when parsing
    xmlError,          ## an error occurred during parsing
    xmlEof,            ## end of file reached
    xmlCharData,       ## character data
    xmlWhitespace,     ## whitespace has been parsed
    xmlComment,        ## a comment has been parsed
    xmlPI,             ## processing instruction (`<?name something ?>`)
    xmlElementStart,   ## `<elem>`
    xmlElementEnd,     ## `</elem>`
    xmlElementOpen,    ## `<elem
    xmlAttribute,      ## `key = "value"` pair
    xmlElementClose,   ## `>`
    xmlCData,          ## `<![CDATA[` ... data ... `]]>`
    xmlEntity,         ## &entity;
    xmlSpecial         ## `<! ... data ... >`

  XmlErrorKind* = enum        ## enumeration that lists all errors that can occur
    errNone,                  ## no error
    errEndOfCDataExpected,    ## `]]>` expected
    errNameExpected,          ## name expected
    errSemicolonExpected,     ## `;` expected
    errQmGtExpected,          ## `?>` expected
    errGtExpected,            ## `>` expected
    errEqExpected,            ## `=` expected
    errQuoteExpected,         ## `"` or `'` expected
    errEndOfCommentExpected   ## `-->` expected
    errAttributeValueExpected ## non-empty attribute value expected

  ParserState = enum
    stateStart, stateNormal, stateAttr, stateEmptyElementTag, stateError

  XmlParseOption* = enum ## options for the XML parser
    reportWhitespace,    ## report whitespace
    reportComments       ## report comments
    allowUnquotedAttribs ## allow unquoted attribute values (for HTML)
    allowEmptyAttribs    ## allow empty attributes (without explicit value)

  XmlParser* = object of BaseLexer ## the parser object.
    a, b, c: string
    kind: XmlEventKind
    err: XmlErrorKind
    state: ParserState
    cIsEmpty: bool
    filename: string
    options: set[XmlParseOption]

const
  errorMessages: array[XmlErrorKind, string] = [
    "no error",
    "']]>' expected",
    "name expected",
    "';' expected",
    "'?>' expected",
    "'>' expected",
    "'=' expected",
    "'\"' or \"'\" expected",
    "'-->' expected",
    "attribute value expected"
  ]

proc open*(my: var XmlParser, input: Stream, filename: string,
           options: set[XmlParseOption] = {}) =
  ## initializes the parser with an input stream. `Filename` is only used
  ## for nice error messages. The parser's behaviour can be controlled by
  ## the `options` parameter: If `options` contains `reportWhitespace`
  ## a whitespace token is reported as an `xmlWhitespace` event.
  ## If `options` contains `reportComments` a comment token is reported as an
  ## `xmlComment` event.
  lexbase.open(my, input, 8192, {'\c', '\L', '/'})
  my.filename = filename
  my.state = stateStart
  my.kind = xmlError
  my.a = ""
  my.b = ""
  my.c = ""
  my.cIsEmpty = true
  my.options = options

proc close*(my: var XmlParser) {.inline.} =
  ## closes the parser `my` and its associated input stream.
  lexbase.close(my)

func kind*(my: XmlParser): XmlEventKind {.inline.} =
  ## returns the current event type for the XML parser
  my.kind

func charData*(my: XmlParser): string {.inline.} =
  ## returns the character data for the events: `xmlCharData`,
  ## `xmlWhitespace`, `xmlComment`, `xmlCData`, `xmlSpecial`
  ## Raises an assertion in debug mode if `my.kind` is not one
  ## of those events. In release mode, this will not trigger an error
  ## but the value returned will not be valid.
  assert(my.kind in {xmlCharData, xmlWhitespace, xmlComment, xmlCData,
                     xmlSpecial})
  my.a

func elementName*(my: XmlParser): string {.inline.} =
  ## returns the element name for the events: `xmlElementStart`,
  ## `xmlElementEnd`, `xmlElementOpen`
  ## Raises an assertion in debug mode if `my.kind` is not one
  ## of those events. In release mode, this will not trigger an error
  ## but the value returned will not be valid.
  assert(my.kind in {xmlElementStart, xmlElementEnd, xmlElementOpen})
  my.a

func entityName*(my: XmlParser): string {.inline.} =
  ## returns the entity name for the event: `xmlEntity`
  ## Raises an assertion in debug mode if `my.kind` is not
  ## `xmlEntity`. In release mode, this will not trigger an error
  ## but the value returned will not be valid.
  assert(my.kind == xmlEntity)
  my.a

func attrKey*(my: XmlParser): string {.inline.} =
  ## returns the attribute key for the event `xmlAttribute`
  ## Raises an assertion in debug mode if `my.kind` is not
  ## `xmlAttribute`. In release mode, this will not trigger an error
  ## but the value returned will not be valid.
  assert(my.kind == xmlAttribute)
  my.a

func attrValue*(my: XmlParser): string {.inline.} =
  ## returns the attribute value for the event `xmlAttribute`
  ## Raises an assertion in debug mode if `my.kind` is not
  ## `xmlAttribute`. In release mode, this will not trigger an error
  ## but the value returned will not be valid.
  assert(my.kind == xmlAttribute)
  my.b

func piName*(my: XmlParser): string {.inline.} =
  ## returns the processing instruction name for the event `xmlPI`
  ## Raises an assertion in debug mode if `my.kind` is not
  ## `xmlPI`. In release mode, this will not trigger an error
  ## but the value returned will not be valid.
  assert(my.kind == xmlPI)
  my.a

func piRest*(my: XmlParser): string {.inline.} =
  ## returns the rest of the processing instruction for the event `xmlPI`
  ## Raises an assertion in debug mode if `my.kind` is not
  ## `xmlPI`. In release mode, this will not trigger an error
  ## but the value returned will not be valid.
  assert(my.kind == xmlPI)
  my.b

func rawData*(my: var XmlParser): lent string {.inline.} =
  ## returns the underlying 'data' string by reference.
  ## This is only used for speed hacks.
  my.a

func rawData2*(my: var XmlParser): lent string {.inline.} =
  ## returns the underlying second 'data' string by reference.
  ## This is only used for speed hacks.
  my.b

func getColumn*(my: XmlParser): int {.inline.} =
  ## get the current column the parser has arrived at.
  getColNumber(my, my.bufpos)

func getLine*(my: XmlParser): int {.inline.} =
  ## get the current line the parser has arrived at.
  my.lineNumber

func getFilename*(my: XmlParser): string {.inline.} =
  ## get the filename of the file that the parser processes.
  my.filename

func errorMsg*(my: XmlParser): string =
  ## returns a helpful error message for the event `xmlError`
  assert(my.kind == xmlError)
  var msg = ""
  if ord(my.err) >= 0 and ord(my.err) <= ord(high(XmlErrorKind)):
    msg = errorMessages[my.err]
  my.filename & "(" & $getLine(my) & ", " & $getColumn(my) &
    ") Error: " & msg

func errorMsgExpected*(my: XmlParser, tag: string): string =
  ## returns an error message "<tag> expected" in the same format as the
  ## other error messages
  my.filename & "(" & $getLine(my) & ", " & $getColumn(my) &
    ") Error: <" & tag & "> expected"

func errorMsg*(my: XmlParser, msg: string): string =
  ## returns an error message with text `msg` in the same format as the
  ## other error messages
  my.filename & "(" & $getLine(my) & ", " & $getColumn(my) &
    ") Error: " & msg

func markError(my: var XmlParser, kind: XmlErrorKind) {.inline.} =
  my.err = kind
  my.state = stateError

proc parseCDATA(my: var XmlParser) =
  var pos = my.bufpos + len("<![CDATA[")
  while true:
    case charAt(my.buf, pos)
    of ']':
      if charAt(my.buf, pos+1) == ']' and charAt(my.buf, pos+2) == '>':
        inc(pos, 3)
        break
      add(my.a, ']')
      inc(pos)
    of '\0':
      markError(my, errEndOfCDataExpected)
      break
    of '\c':
      pos = lexbase.handleCR(my, pos)
      add(my.a, '\L')
    of '\L':
      pos = lexbase.handleLF(my, pos)
      add(my.a, '\L')
    of '/':
      pos = lexbase.handleRefillChar(my, pos)
      add(my.a, '/')
    else:
      add(my.a, charAt(my.buf, pos))
      inc(pos)
  my.bufpos = pos # store back
  my.kind = xmlCData

proc parseComment(my: var XmlParser) =
  var pos = my.bufpos + len("<!--")
  while true:
    case charAt(my.buf, pos)
    of '-':
      if charAt(my.buf, pos+1) == '-' and charAt(my.buf, pos+2) == '>':
        inc(pos, 3)
        break
      if my.options.contains(reportComments): add(my.a, '-')
      inc(pos)
    of '\0':
      markError(my, errEndOfCommentExpected)
      break
    of '\c':
      pos = lexbase.handleCR(my, pos)
      if my.options.contains(reportComments): add(my.a, '\L')
    of '\L':
      pos = lexbase.handleLF(my, pos)
      if my.options.contains(reportComments): add(my.a, '\L')
    of '/':
      pos = lexbase.handleRefillChar(my, pos)
      if my.options.contains(reportComments): add(my.a, '/')
    else:
      if my.options.contains(reportComments): add(my.a, charAt(my.buf, pos))
      inc(pos)
  my.bufpos = pos
  my.kind = xmlComment

proc parseWhitespace(my: var XmlParser, skip = false) =
  var pos = my.bufpos
  while true:
    case charAt(my.buf, pos)
    of ' ', '\t':
      if not skip: add(my.a, charAt(my.buf, pos))
      inc(pos)
    of '\c':
      # the specification says that CR-LF, CR are to be transformed to LF
      pos = lexbase.handleCR(my, pos)
      if not skip: add(my.a, '\L')
    of '\L':
      pos = lexbase.handleLF(my, pos)
      if not skip: add(my.a, '\L')
    else:
      break
  my.bufpos = pos

const
  NameStartChar = {'A'..'Z', 'a'..'z', '_', ':', '\128'..'\255'}
  NameChar = {'A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_', ':', '\128'..'\255'}

proc parseName(buf: openArray[char]; pos: int; dest: var string): int =
  ## Appends the name starting at `pos` to `dest` and returns the position
  ## after it. If no name starts there, `dest` is unchanged and `pos` is
  ## returned, leaving the error report to the caller.
  result = pos
  if charAt(buf, result) in NameStartChar:
    while true:
      add(dest, charAt(buf, result))
      inc(result)
      if charAt(buf, result) notin NameChar: break

proc parseEntity(buf: openArray[char]; pos: int; dest: var string;
                 kind: var XmlEventKind; err: var XmlErrorKind;
                 state: var ParserState): int =
  ## Appends the decoded entity starting at `pos` to `dest` and returns the
  ## position after it. The parser fields it used to write are passed
  ## separately so the destination buffer does not alias the parser.
  result = pos + 1
  kind = xmlCharData
  if charAt(buf, result) == '#':
    const maxCodePoint = 0x10FFFF
    var r: int = 0
    inc(result)
    if charAt(buf, result) == 'x':
      inc(result)
      while true:
        var d: int = 0
        case charAt(buf, result)
        of '0'..'9': d = ord(charAt(buf, result)) - ord('0')
        of 'a'..'f': d = ord(charAt(buf, result)) - ord('a') + 10
        of 'A'..'F': d = ord(charAt(buf, result)) - ord('A') + 10
        else: break
        # saturate so that overlong numbers cannot overflow
        if r <= maxCodePoint: r = (r shl 4) or d
        inc(result)
    else:
      while charAt(buf, result) in {'0'..'9'}:
        if r <= maxCodePoint: r = r * 10 + (ord(charAt(buf, result)) - ord('0'))
        inc(result)
    if r > maxCodePoint or (r >= 0xD800 and r <= 0xDFFF):
      # not a Unicode scalar value: use the replacement character
      r = 0xFFFD
    add(dest, toUTF8(Rune(r)))
  elif charAt(buf, result) == 'l' and charAt(buf, result+1) == 't' and charAt(buf, result+2) == ';':
    add(dest, '<')
    inc(result, 2)
  elif charAt(buf, result) == 'g' and charAt(buf, result+1) == 't' and charAt(buf, result+2) == ';':
    add(dest, '>')
    inc(result, 2)
  elif charAt(buf, result) == 'a' and charAt(buf, result+1) == 'm' and charAt(buf, result+2) == 'p' and
      charAt(buf, result+3) == ';':
    add(dest, '&')
    inc(result, 3)
  elif charAt(buf, result) == 'a' and charAt(buf, result+1) == 'p' and charAt(buf, result+2) == 'o' and
      charAt(buf, result+3) == 's' and charAt(buf, result+4) == ';':
    add(dest, '\'')
    inc(result, 4)
  elif charAt(buf, result) == 'q' and charAt(buf, result+1) == 'u' and charAt(buf, result+2) == 'o' and
      charAt(buf, result+3) == 't' and charAt(buf, result+4) == ';':
    add(dest, '"')
    inc(result, 4)
  else:
    let nameStart = result
    var name = ""
    result = parseName(buf, result, name)
    if result == nameStart:
      err = errNameExpected
      state = stateError
    elif err != errNameExpected and charAt(buf, result) == ';':
      kind = xmlEntity
    else:
      add(dest, '&')
    add(dest, name)
  if charAt(buf, result) == ';':
    inc(result)
  else:
    err = errSemicolonExpected
    # do not overwrite `state` here, it's a benign error

proc parsePI(my: var XmlParser) =
  inc(my.bufpos, "<?".len)
  let nameStart = my.bufpos
  my.bufpos = parseName(my.buf, nameStart, my.a)
  if my.bufpos == nameStart: markError(my, errNameExpected)
  var pos = my.bufpos
  setLen(my.b, 0)
  while true:
    case charAt(my.buf, pos)
    of '\0':
      markError(my, errQmGtExpected)
      break
    of '?':
      if charAt(my.buf, pos+1) == '>':
        inc(pos, 2)
        break
      add(my.b, '?')
      inc(pos)
    of '\c':
      # the specification says that CR-LF, CR are to be transformed to LF
      pos = lexbase.handleCR(my, pos)
      add(my.b, '\L')
    of '\L':
      pos = lexbase.handleLF(my, pos)
      add(my.b, '\L')
    of '/':
      pos = lexbase.handleRefillChar(my, pos)
      add(my.b, '/')
    else:
      add(my.b, charAt(my.buf, pos))
      inc(pos)
  my.bufpos = pos
  my.kind = xmlPI

proc parseSpecial(my: var XmlParser) =
  # things that start with <!
  var pos = my.bufpos + 2
  var opentags = 0
  while true:
    case charAt(my.buf, pos)
    of '\0':
      markError(my, errGtExpected)
      break
    of '<':
      inc(opentags)
      inc(pos)
      add(my.a, '<')
    of '>':
      if opentags <= 0:
        inc(pos)
        break
      dec(opentags)
      inc(pos)
      add(my.a, '>')
    of '\c':
      pos = lexbase.handleCR(my, pos)
      add(my.a, '\L')
    of '\L':
      pos = lexbase.handleLF(my, pos)
      add(my.a, '\L')
    of '/':
      pos = lexbase.handleRefillChar(my, pos)
      add(my.a, '/')
    else:
      add(my.a, charAt(my.buf, pos))
      inc(pos)
  my.bufpos = pos
  my.kind = xmlSpecial

proc parseTag(my: var XmlParser) =
  inc(my.bufpos)
  let nameStart = my.bufpos
  my.bufpos = parseName(my.buf, nameStart, my.a)
  if my.bufpos == nameStart: markError(my, errNameExpected)
  # if we have no name, do not interpret the '<':
  if my.a.len == 0:
    my.kind = xmlCharData
    add(my.a, '<')
    return
  parseWhitespace(my, skip = true)
  if charAt(my.buf, my.bufpos) in NameStartChar:
    # an attribute follows:
    my.kind = xmlElementOpen
    my.state = stateAttr
    my.c = my.a # save for later
    my.cIsEmpty = false
  else:
    my.kind = xmlElementStart
    let slash = charAt(my.buf, my.bufpos) == '/'
    if slash:
      my.bufpos = lexbase.handleRefillChar(my, my.bufpos)
    if slash and charAt(my.buf, my.bufpos) == '>':
      inc(my.bufpos)
      my.state = stateEmptyElementTag
      my.c = ""
      my.cIsEmpty = true
    elif charAt(my.buf, my.bufpos) == '>':
      inc(my.bufpos)
    else:
      markError(my, errGtExpected)

proc parseEndTag(my: var XmlParser) =
  my.bufpos = lexbase.handleRefillChar(my, my.bufpos+1)
  #inc(my.bufpos, 2)
  let nameStart = my.bufpos
  my.bufpos = parseName(my.buf, nameStart, my.a)
  if my.bufpos == nameStart: markError(my, errNameExpected)
  parseWhitespace(my, skip = true)
  if charAt(my.buf, my.bufpos) == '>':
    inc(my.bufpos)
  else:
    markError(my, errGtExpected)
  my.kind = xmlElementEnd

proc parseAttribute(my: var XmlParser) =
  my.kind = xmlAttribute
  setLen(my.a, 0)
  setLen(my.b, 0)
  let nameStart = my.bufpos
  my.bufpos = parseName(my.buf, nameStart, my.a)
  if my.bufpos == nameStart: markError(my, errNameExpected)
  # if we have no name, we have '<tag attr= key %&$$%':
  if my.a.len == 0:
    markError(my, errGtExpected)
    return

  let startPos = my.bufpos
  parseWhitespace(my, skip = true)
  if charAt(my.buf, my.bufpos) != '=':
    if allowEmptyAttribs notin my.options or
        (charAt(my.buf, my.bufpos) != '>' and my.bufpos == startPos):
      markError(my, errEqExpected)
    return

  inc(my.bufpos)
  parseWhitespace(my, skip = true)

  var pos = my.bufpos
  if charAt(my.buf, pos) in {'\'', '"'}:
    var quote = charAt(my.buf, pos)
    var pendingSpace = false
    inc(pos)
    while true:
      case charAt(my.buf, pos)
      of '\0':
        markError(my, errQuoteExpected)
        break
      of '&':
        if pendingSpace:
          add(my.b, ' ')
          pendingSpace = false
        my.bufpos = parseEntity(my.buf, pos, my.b, my.kind, my.err, my.state)
        my.kind = xmlAttribute # parseEntity overwrites my.kind!
        pos = my.bufpos
      of ' ', '\t':
        pendingSpace = true
        inc(pos)
      of '\c':
        pos = lexbase.handleCR(my, pos)
        pendingSpace = true
      of '\L':
        pos = lexbase.handleLF(my, pos)
        pendingSpace = true
      of '/':
        if pendingSpace:
          add(my.b, ' ')
          pendingSpace = false
        pos = lexbase.handleRefillChar(my, pos)
        add(my.b, '/')
      else:
        if charAt(my.buf, pos) == quote:
          inc(pos)
          break
        else:
          if pendingSpace:
            add(my.b, ' ')
            pendingSpace = false
          add(my.b, charAt(my.buf, pos))
          inc(pos)
  elif allowUnquotedAttribs in my.options:
    const disallowedChars = {'"', '\'', '`', '=', '<', '>', ' ',
                             '\0', '\t', '\L', '\F', '\f'}
    let startPos = pos
    while (let c = charAt(my.buf, pos); c notin disallowedChars):
      if c == '&':
        my.bufpos = parseEntity(my.buf, pos, my.b, my.kind, my.err, my.state)
        my.kind = xmlAttribute # parseEntity overwrites my.kind!
        pos = my.bufpos
      elif c == '/':
        pos = lexbase.handleRefillChar(my, pos)
        add(my.b, '/')
      else:
        add(my.b, c)
        inc(pos)
    if pos == startPos:
      markError(my, errAttributeValueExpected)
  else:
    markError(my, errQuoteExpected)
    # error corrections: guess what was meant
    while charAt(my.buf, pos) != '>' and charAt(my.buf, pos) > ' ':
      add(my.b, charAt(my.buf, pos))
      inc pos
  my.bufpos = pos
  parseWhitespace(my, skip = true)

proc parseCharData(my: var XmlParser) =
  var pos = my.bufpos
  while true:
    case charAt(my.buf, pos)
    of '\0', '<', '&': break
    of '\c':
      # the specification says that CR-LF, CR are to be transformed to LF
      pos = lexbase.handleCR(my, pos)
      add(my.a, '\L')
    of '\L':
      pos = lexbase.handleLF(my, pos)
      add(my.a, '\L')
    of '/':
      pos = lexbase.handleRefillChar(my, pos)
      add(my.a, '/')
    else:
      add(my.a, charAt(my.buf, pos))
      inc(pos)
  my.bufpos = pos
  my.kind = xmlCharData

proc rawGetTok(my: var XmlParser) =
  my.kind = xmlError
  setLen(my.a, 0)
  var pos = my.bufpos
  case charAt(my.buf, pos)
  of '<':
    case charAt(my.buf, pos+1)
    of '/':
      parseEndTag(my)
    of '!':
      if charAt(my.buf, pos+2) == '[' and charAt(my.buf, pos+3) == 'C' and
          charAt(my.buf, pos+4) == 'D' and charAt(my.buf, pos+5) == 'A' and
          charAt(my.buf, pos+6) == 'T' and charAt(my.buf, pos+7) == 'A' and
          charAt(my.buf, pos+8) == '[':
        parseCDATA(my)
      elif charAt(my.buf, pos+2) == '-' and charAt(my.buf, pos+3) == '-':
        parseComment(my)
      else:
        parseSpecial(my)
    of '?':
      parsePI(my)
    else:
      parseTag(my)
  of ' ', '\t', '\c', '\l':
    parseWhitespace(my)
    my.kind = xmlWhitespace
  of '\0':
    my.kind = xmlEof
  of '&':
    my.bufpos = parseEntity(my.buf, my.bufpos, my.a, my.kind, my.err, my.state)
  else:
    parseCharData(my)
  assert my.kind != xmlError

proc getTok(my: var XmlParser) =
  while true:
    let lastKind = my.kind
    rawGetTok(my)
    case my.kind
    of xmlComment:
      if my.options.contains(reportComments): break
    of xmlWhitespace:
      if my.options.contains(reportWhitespace) or lastKind in {xmlCharData,
          xmlComment, xmlEntity}:
        break
    else: break

proc next*(my: var XmlParser) =
  ## retrieves the first/next event. This controls the parser.
  case my.state
  of stateNormal:
    getTok(my)
  of stateStart:
    my.state = stateNormal
    getTok(my)
    if my.kind == xmlPI and my.a == "xml":
      # just skip the first `<?xml >` processing instruction
      getTok(my)
  of stateAttr:
    # parse an attribute key-value pair:
    if charAt(my.buf, my.bufpos) == '>':
      my.kind = xmlElementClose
      inc(my.bufpos)
      my.state = stateNormal
    elif charAt(my.buf, my.bufpos) == '/':
      my.bufpos = lexbase.handleRefillChar(my, my.bufpos)
      if charAt(my.buf, my.bufpos) == '>':
        my.kind = xmlElementClose
        inc(my.bufpos)
        my.state = stateEmptyElementTag
      else:
        markError(my, errGtExpected)
    else:
      parseAttribute(my)
      # state remains the same
  of stateEmptyElementTag:
    my.state = stateNormal
    my.kind = xmlElementEnd
    if not my.cIsEmpty:
      my.a = my.c
  of stateError:
    my.kind = xmlError
    my.state = stateNormal

when not defined(testing) and isMainModule:
  import std/[os, syncio]
  var s = newFileStream(paramStr(1), fmRead)
  if s == nil: quit("cannot open the file" & paramStr(1))
  var x: XmlParser
  open(x, s, paramStr(1))
  while true:
    next(x)
    case x.kind
    of xmlError: echo(x.errorMsg())
    of xmlEof: break
    of xmlCharData: echo(x.charData)
    of xmlWhitespace: echo("|" & x.charData & "|")
    of xmlComment: echo("<!-- " & x.charData & " -->")
    of xmlPI: echo("<? " & x.piName & " ## " & x.piRest & " ?>")
    of xmlElementStart: echo("<" & x.elementName & ">")
    of xmlElementEnd: echo("</" & x.elementName & ">")

    of xmlElementOpen: echo("<" & x.elementName)
    of xmlAttribute:
      echo("Key: " & x.attrKey)
      echo("Value: " & x.attrValue)

    of xmlElementClose: echo(">")
    of xmlCData:
      echo("<![CDATA[" & x.charData & "]]>")
    of xmlEntity:
      echo("&" & x.entityName & ";")
    of xmlSpecial:
      echo("SPECIAL: " & x.charData)
  close(x)
