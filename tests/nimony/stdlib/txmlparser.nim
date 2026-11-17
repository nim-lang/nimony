import std/[assertions, streams, syncio, xmlparser, xmltree]

proc main() {.raises.} =
  block: # elements, attributes, text and markup nodes
    let tree = parseXml(
      "<root a=\"1\"><child>hello</child><!-- c --><![CDATA[<raw>]]>&amp;</root>")
    assert tree != nil
    assert tree.tag == "root"
    assert tree.attr("a") == "1"
    assert len(tree) == 4
    assert tree[0].tag == "child"
    assert tree[0].innerText == "hello"
    assert tree[1].kind == xnComment
    assert tree[1].text == " c "
    assert tree[2].kind == xnCData
    assert tree[2].text == "<raw>"
    assert tree[3].kind == xnText
    assert tree[3].text == "&"

  block: # comments and processing instructions are optional
    let withoutComments = parseXml("<root><!-- hidden --></root>", {})
    assert withoutComments != nil
    assert len(withoutComments) == 0
    let withPi = parseXml("<?xml version=\"1.0\"?><root/>")
    assert withPi != nil
    assert withPi.tag == "root"

  block: # sink input is copied when the caller keeps using it
    var xml = "<root/>"
    let tree = parseXml(xml)
    assert tree != nil
    assert xml == "<root/>"

  block: # recovered errors are reported, and the bad overload raises
    var errors = newSeq[string]()
    let bad = parseXml(newStringStream("<root><child></root>"), "bad.xml", errors)
    assert bad != nil
    assert errors.len > 0

    var err = Success
    try:
      discard parseXml("<root>")
    except ErrorCode as e:
      err = e
    assert err == SyntaxError

  block: # nesting deeper than xmlMaxDepth is rejected
    var deep = ""
    var i = 0
    while i <= xmlMaxDepth:
      deep.add("<a>")
      inc i
    i = 0
    while i <= xmlMaxDepth:
      deep.add("</a>")
      inc i

    var err = Success
    try:
      discard parseXml(deep)
    except ErrorCode as e:
      err = e
    assert err == SyntaxError

    var errors = newSeq[string]()
    let deepTree = parseXml(newStringStream(deep), "deep.xml", errors)
    assert deepTree == nil
    assert errors.len > 0

  block: # the error-sequence overload keeps the raw IO error
    var errors = newSeq[string]()
    var caught = false
    try:
      discard loadXml("/definitely/not/a/file.xml", errors)
    except ErrorCode:
      caught = true
    assert caught

  block: # failed file loads raise IOError through the exception overload
    var err = Success
    try:
      discard loadXml("/definitely/not/a/file.xml")
    except ErrorCode as e:
      err = e
    assert err == IOError

try:
  main()
except:
  quit "txmlparser: unexpected exception"

echo "xmlparser: OK"
