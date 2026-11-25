import std/[assertions, syncio, xmltree]

proc main() =
  block: # constructors and rendering
    var g = newElement("myTag")
    g.add newText("some text")
    g.add newComment("this is comment")

    var h = newElement("secondTag")
    h.add newEntity("some entity")

    let att = {"key1": "first value", "key2": "second value"}.toXmlAttributes
    let k = newXmlTree("treeTag", [g, h], att)
    assert $k == """<treeTag key2="second value" key1="first value">
  <myTag>some text<!-- this is comment --></myTag>
  <secondTag>&some entity;</secondTag>
</treeTag>"""

  block: # accessors, attributes and client data
    var t = newComment("text")
    t.text = "other text"
    assert t.text == "other text"
    assert rawText(t) == "other text"

    var n = newElement("tag")
    n.tag = "other"
    assert n.tag == "other"
    assert rawTag(n) == "other"
    n.clientData = 42
    assert n.clientData == 42

    n.attrs = {"a": "1", "b": "2"}.toXmlAttributes
    assert n.attrsLen == 2
    assert n.attr("a") == "1"
    assert n.attr("missing") == ""
    assert n.attrs != nil

    n.attrs = toXmlAttributes(("x", "1"), ("y", "2"))
    assert n.attrsLen == 2
    assert n.attr("x") == "1"
    assert n.attr("y") == "2"

  block: # text, escaping and inner text
    var n = newElement("root")
    n.add newText("a<b&c")
    n.add newCData("<raw>")
    n.add newVerbatimText("<verbatim>")
    n.add newEntity("amp")
    assert innerText(n) == "a<b&camp"
    assert escape("a<b&c\"'") == "a&lt;b&amp;c&quot;&apos;"
    var s = ""
    s.addEscaped("a<b")
    assert s == "a&lt;b"
    assert $n == "<root>a&lt;b&amp;c<![CDATA[<raw>]]><verbatim>&amp;</root>"

  block: # mutation operations
    var f = newElement("myTag")
    f.add newElement("first")
    f.insert(newElement("second"), 0)
    assert len(f) == 2
    assert f.kind == xnElement
    assert $f == """<myTag>
  <second />
  <first />
</myTag>"""

    f.insert([newElement("third"), newElement("fourth")], 1)
    f.delete(1..2)
    assert $f == """<myTag>
  <second />
  <first />
</myTag>"""

    f.replace(1, @[newElement("replacement")])
    assert $f == """<myTag>
  <second />
  <replacement />
</myTag>"""

    f.replace(0..0, @[newElement("third"), newElement("fourth")])
    assert $f == """<myTag>
  <third />
  <fourth />
  <replacement />
</myTag>"""

    f[0] = newElement("changed")
    assert f[0].tag == "changed"
    var count = 0
    for child in items(f):
      assert child.kind == xnElement
      inc count
    assert count == 3
    for child in mitems(f):
      child = newElement("mut")
    assert f[0].tag == "mut"
    f.clear()
    assert len(f) == 0

    var q = newElement("q")
    q.add([newText("a"), newComment("b")])
    assert $q == "<q>a<!-- b --></q>"

  block: # searching
    var
      b = newElement("good")
      c = newElement("bad")
      d = newElement("BAD")
      e = newElement("GOOD")
    b.add newText("b text")
    c.add newText("c text")
    d.add newText("d text")
    e.add newText("e text")
    let a = newXmlTree("father", [b, c, d, e])
    let bad = a.child("bad")
    assert bad != nil
    assert $bad == "<bad>c text</bad>"
    assert $a.child("missing") == ""
    assert $a.findAll("good") == "@[<good>b text</good>]"
    assert $a.findAll("BAD") == "@[<BAD>d text</BAD>]"
    assert $a.findAll("good", caseInsensitive = true) ==
      "@[<good>b text</good>, <GOOD>e text</GOOD>]"
    assert $a.findAll("BAD", caseInsensitive = true) ==
      "@[<bad>c text</bad>, <BAD>d text</BAD>]"

  block: # constructor macro
    let n = <>a(href="https://nim-lang.org", newText("Nim rules."))
    assert $n == """<a href="https://nim-lang.org">Nim rules.</a>"""

    let m = <>a(href="https://nim-lang.org", title="Nim",
      newText("Nim rules."), <>b(newText("child")))
    assert m.attr("href") == "https://nim-lang.org"
    assert m.attr("title") == "Nim"
    assert len(m) == 2
    assert m[1].tag == "b"

    let s = <>span(newText("text"))
    assert $s == "<span>text</span>"

  assert xmlHeader == "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"

main()
echo "xmltree: OK"
