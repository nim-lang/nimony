import std/[assertions, parsexml, streams, syncio]

proc events(s: string; options: set[XmlParseOption] = {}): seq[string] {.raises.} =
  ## Renders the parser's event stream so the state machine can be asserted
  ## without depending on the output formatting.
  result = @[]
  var p = default(XmlParser)
  let input = newStringStream(s)
  open(p, input, "mem.xml", options)
  while true:
    next(p)
    case p.kind
    of xmlError:
      result.add "error"
      break
    of xmlEof: break
    of xmlCharData: result.add "text:" & p.charData
    of xmlWhitespace: result.add "ws:" & p.charData
    of xmlComment: result.add "comment:" & p.charData
    of xmlPI: result.add "pi:" & p.piName & "|" & p.piRest
    of xmlElementStart: result.add "start:" & p.elementName
    of xmlElementEnd: result.add "end:" & p.elementName
    of xmlElementOpen: result.add "open:" & p.elementName
    of xmlAttribute: result.add "attr:" & p.attrKey & "=" & p.attrValue
    of xmlElementClose: result.add "close"
    of xmlCData: result.add "cdata:" & p.charData
    of xmlEntity: result.add "entity:" & p.entityName
    of xmlSpecial: result.add "special:" & p.charData
  close(p)

proc main() {.raises.} =
  block: # elements, text and the XML declaration
    assert events("""<?xml version="1.0"?><root><child>hello</child><empty/></root>""") ==
      @["start:root", "start:child", "text:hello", "end:child",
        "start:empty", "end:empty", "end:root"]

  block: # attributes, quoted values and a self-closing element
    assert events("""<root a="1" b='two'>x&amp;y</root>""") ==
      @["open:root", "attr:a=1", "attr:b=two", "close",
        "text:x", "text:&", "text:y", "end:root"]
    assert events("<p>a<br/>b</p>") ==
      @["start:p", "text:a", "start:br", "end:br", "text:b", "end:p"]
    assert events("""<p x="a / b"/>""") ==
      @["open:p", "attr:x=a / b", "close", "end:p"]

  block: # entities decode to character data; unknown entities stay events
    assert events("""<root>&lt;&gt;&amp;&apos;&quot;&#65;&#x42;&unknown;</root>""") ==
      @["start:root", "text:<", "text:>", "text:&", "text:'", "text:\"",
        "text:A", "text:B", "entity:unknown", "end:root"]

  block: # invalid numeric references become U+FFFD instead of overflowing
    assert events("<root>&#x110000;</root>") ==
      @["start:root", "text:\xEF\xBF\xBD", "end:root"]
    assert events("<root>&#xD800;</root>") ==
      @["start:root", "text:\xEF\xBF\xBD", "end:root"]
    assert events("<root>&#999999999999999999999999;</root>") ==
      @["start:root", "text:\xEF\xBF\xBD", "end:root"]
    assert events("<root>&#xFFFFFFFFFFFFFFFF;</root>") ==
      @["start:root", "text:\xEF\xBF\xBD", "end:root"]

  block: # comments, CDATA, processing instructions and special markup
    assert events("""<root><!-- hi --><![CDATA[<raw>]]><?pi rest?><!DOCTYPE note></root>""",
                  {reportComments}) ==
      @["start:root", "comment: hi ", "cdata:<raw>", "pi:pi| rest",
        "special:DOCTYPE note", "end:root"]
    assert events("""<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN">""") ==
      @["special:DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\""]
    assert events("<root><!-- hidden --></root>") == @["start:root", "end:root"]
    assert events("<?target data?><root/>") ==
      @["pi:target| data", "start:root", "end:root"]

  block: # whitespace reporting
    assert events("<root> <child/> </root>") ==
      @["start:root", "start:child", "end:child", "end:root"]
    assert events(" <root> </root> ", {reportWhitespace}) ==
      @["ws: ", "start:root", "ws: ", "end:root", "ws: "]

  block: # HTML-friendly attribute options
    assert events("<p x=one y=two>", {allowUnquotedAttribs}) ==
      @["open:p", "attr:x=one", "attr:y=two", "close"]
    assert events("<p x y>", {allowEmptyAttribs}) ==
      @["open:p", "attr:x=", "attr:y=", "close"]

  block: # error position and diagnostics
    var p = default(XmlParser)
    open(p, newStringStream("<root a=\"unterminated"), "bad.xml")
    while true:
      next(p)
      if p.kind == xmlError: break
    assert p.kind == xmlError
    assert p.getFilename() == "bad.xml"
    assert p.getLine() == 1
    assert p.getColumn() == 21
    assert p.errorMsg() == "bad.xml(1, 21) Error: '\"' or \"'\" expected"
    assert p.errorMsgExpected("/root") ==
      "bad.xml(1, 21) Error: </root> expected"
    assert p.errorMsg("custom") == "bad.xml(1, 21) Error: custom"
    close(p)

  block: # raw data accessors expose the current token buffers
    var p = default(XmlParser)
    open(p, newStringStream("<root><?pi rest?></root>"), "raw.xml")
    next(p) # <root>
    next(p) # <?pi rest?>
    assert p.kind == xmlPI
    assert p.rawData() == "pi"
    assert p.rawData2() == " rest"
    close(p)

try:
  main()
except:
  quit "tparsexml: unexpected exception"

echo "parsexml: OK"
