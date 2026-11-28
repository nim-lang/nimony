#
#
#            Nim's Runtime Library
#        (c) Copyright 2010 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module parses an XML document and creates its XML tree representation.
##
## Nimony models errors as `ErrorCode` values: malformed XML and excessive
## nesting raise `SyntaxError`; a file that cannot be opened raises `IOError`.
## The overloads taking an `errors` sequence still collect the detailed parser
## messages.

import std/[assertions, streams, parsexml, strtabs, xmltree]

const
  xmlMaxDepth* {.intdefine.} = 256 ## The maximum nesting depth of elements, like libxml2.
    ## Deeper documents are rejected, as they could exhaust the stack.
    ## This can be set on the command line via `-d:xmlMaxDepth=N`.

proc addNode(father: XmlNode, son: nil XmlNode) =
  if son != nil: add(father, son)

proc parse(x: var XmlParser, errors: var seq[string];
           depth: int; tooDeep: var bool): nil XmlNode {.gcsafe.}

proc untilElementEnd(x: var XmlParser, result: XmlNode,
                     errors: var seq[string]; depth: int;
                     tooDeep: var bool) =
  if depth > xmlMaxDepth:
    tooDeep = true
    return
  while not tooDeep:
    case x.kind
    of xmlElementEnd:
      if x.elementName == result.tag:
        next(x)
      else:
        errors.add(errorMsg(x, "</" & result.tag & "> expected"))
        # do not skip it here!
      break
    of xmlEof:
      errors.add(errorMsg(x, "</" & result.tag & "> expected"))
      break
    else:
      result.addNode(parse(x, errors, depth, tooDeep))

proc parse(x: var XmlParser, errors: var seq[string];
           depth: int; tooDeep: var bool): nil XmlNode =
  if tooDeep: return nil
  result = nil
  case x.kind
  of xmlComment:
    result = newComment(x.charData)
    next(x)
  of xmlCharData, xmlWhitespace:
    result = newText(x.charData)
    next(x)
  of xmlPI, xmlSpecial:
    # we just ignore processing instructions for now
    next(x)
  of xmlError:
    errors.add(errorMsg(x))
    next(x)
  of xmlElementStart: ## `<elem>`
    let node = newElement(x.elementName)
    result = node
    next(x)
    untilElementEnd(x, node, errors, depth+1, tooDeep)
  of xmlElementEnd:
    errors.add(errorMsg(x, "unexpected ending tag: " & x.elementName))
  of xmlElementOpen:
    let node = newElement(x.elementName)
    result = node
    next(x)
    let attrs = newStringTable(modeCaseSensitive)
    node.attrs = attrs
    while true:
      case x.kind
      of xmlAttribute:
        attrs[x.attrKey] = x.attrValue
        next(x)
      of xmlElementClose:
        next(x)
        break
      of xmlError:
        errors.add(errorMsg(x))
        next(x)
        break
      else:
        errors.add(errorMsg(x, "'>' expected"))
        next(x)
        break
    untilElementEnd(x, node, errors, depth+1, tooDeep)
  of xmlAttribute, xmlElementClose:
    errors.add(errorMsg(x, "<some_tag> expected"))
    next(x)
  of xmlCData:
    result = newCData(x.charData)
    next(x)
  of xmlEntity:
    ## &entity;
    result = newEntity(x.entityName)
    next(x)
  of xmlEof: discard

proc parseXml*(s: Stream, filename: string,
               errors: var seq[string], options: set[XmlParseOption] = {reportComments}): nil XmlNode =
  ## Parses the XML from stream `s` and returns a `XmlNode`. Every
  ## occurred parsing error is added to the `errors` sequence.
  ## If elements are nested deeper than `xmlMaxDepth`, parsing stops
  ## with an error and `nil` is returned.
  result = nil
  var x = XmlParser()
  open(x, s, filename, options)
  var tooDeep = false
  while true:
    x.next()
    case x.kind
    of xmlElementOpen, xmlElementStart:
      result = parse(x, errors, 0, tooDeep)
      if tooDeep:
        errors.add(errorMsg(x, "elements nested deeper than " & $xmlMaxDepth &
          " levels"))
        result = nil
      break
    of xmlComment, xmlWhitespace, xmlSpecial, xmlPI: discard # just skip it
    of xmlError:
      errors.add(errorMsg(x))
    else:
      errors.add(errorMsg(x, "<some_tag> expected"))
      break
  close(x)

proc parseXml*(s: Stream, options: set[XmlParseOption] = {reportComments}): nil XmlNode {.raises.} =
  ## Parses the XML from stream `s` and returns a `XmlNode`. All parsing
  ## errors are turned into a `SyntaxError` exception.
  var errors = newSeq[string]()
  result = parseXml(s, "unknown_xml_doc", errors, options)
  if errors.len > 0: raise SyntaxError

proc parseXml*(str: sink string, options: set[XmlParseOption] = {reportComments}): nil XmlNode {.raises.} =
  ## Parses the XML from string `str` and returns a `XmlNode`. All parsing
  ## errors are turned into a `SyntaxError` exception.
  parseXml(newStringStream(str), options)

proc loadXml*(path: string, errors: var seq[string], options: set[XmlParseOption] = {reportComments}): nil XmlNode {.raises.} =
  ## Loads and parses XML from file specified by `path`, and returns
  ## a `XmlNode`. Every occurred parsing error is added to the `errors`
  ## sequence. Raises `IOError` if the file cannot be opened.
  var s = newFileStream(path, fmRead)
  if s == nil: raise IOError
  try:
    result = parseXml(s, path, errors, options)
  finally:
    s.close()

proc loadXml*(path: string, options: set[XmlParseOption] = {reportComments}): nil XmlNode {.raises.} =
  ## Loads and parses XML from file specified by `path`, and returns
  ## a `XmlNode`. Raises `SyntaxError` for parsing errors and `IOError`
  ## if the file cannot be opened.
  var errors = newSeq[string]()
  result = loadXml(path, errors, options)
  if errors.len > 0: raise SyntaxError

when isMainModule:
  import std/syncio

  when not defined(testing):
    import std/os

    try:
      var errors = newSeq[string]()
      var x = loadXml(paramStr(1), errors)
      for e in items(errors): echo e

      var f: File
      if open(f, "xmltest.txt", fmWrite):
        f.write($x)
        f.close()
      else:
        quit("cannot write test.txt")
    except ErrorCode:
      quit("cannot read " & paramStr(1))
  else:
    block: # correctly parse ../../tests/testdata/doc1.xml
      let filePath = "tests/testdata/doc1.xml"
      try:
        var errors = newSeq[string]()
        var xml = loadXml(filePath, errors)
        assert(errors.len == 0, "The file tests/testdata/doc1.xml should be parsed without errors.")
      except ErrorCode:
        quit("cannot read " & filePath)

    block bug1518:
      var err = newSeq[string]()
      assert $parseXml(newStringStream"<tag>One &amp; two</tag>", "temp.xml",
          err) == "<tag>One &amp; two</tag>"
