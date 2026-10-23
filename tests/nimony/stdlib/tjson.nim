when defined(nimony):
  {.feature: "untyped".}   # so the `check` template's `xx != b` resolves to bool

import std/[json, syncio, tables]

type Color = enum red, green, blue
type Point = object
  x, y: int
  label: string
  shade: Color
type Box = object
  v: int

template check(a, b) =
  let xx = a
  if xx != b:
    echo "test failed ", astToStr(a), ": got ", xx, " expected ", b
    quit 1

proc sliceLifetime(): JsonNode =
  var t = parseJson("""[10,20,30]""")
  let c = t.root
  result = c[0..1]

proc main() =
  block atoms:
    var t = parseJson("""{"a":1,"b":"hi","c":true,"d":null,"e":3.5}""")
    check len(t.root), 5
    check getInt(t{"a"}), 1
    check getStr(t{"b"}), "hi"
    check getBool(t{"c"}), true
    check kind(t{"d"}), JNull
    check getFloat(t{"e"}), 3.5

  block nested:
    var t = parseJson("""{"x":[1,2,{"y":42}]}""")
    var c = t{"x"}
    check kind(c), JArray
    check len(c), 3
    var elems: seq[int64] = @[]
    for el in items(c):
      var m = el
      if kind(m) == JInt: elems.add getInt(m)
    check elems.len, 2
    check elems[0], 1'i64
    check elems[1], 2'i64

  block read_compat:
    try:
      var t = parseJson("""{"a":[1,2,3],"b":"hi"}""")
      let c = t.root
      check c["a"].kind, JArray
      check c["a"][0].getInt(), 1'i64
      check c["a"][^1].getInt(), 3'i64
      check $c["a"][0..1], "[1,2]"
      check $c["a"][0..^1], "[1,2,3]"
      check $c["a"][^2..^1], "[2,3]"
      check c{"b"}.getStr(), "hi"
      check c{"missing"}.kind, JNull
      check c{0}.kind, JNull
      check c["a"]{1}.getInt(), 2'i64
      check hasKey(c, "a"), true
      check hasKey(c, "z"), false
      check contains(c, "b"), true
      check contains(c["a"], c["a"][0]), true
      check getOrDefault(c, "z").kind, JNull
      check $getOrDefault(c, "z"), "null"
      check pretty(getOrDefault(c, "z")), "null"
      check getBiggestInt(c["a"][0]), 1'i64
      var raised = false
      try:
        discard c["z"]
      except:
        raised = true
      check raised, true

      check $sliceLifetime(), "[10,20]"

      let fields = getFields(c)
      check fields.len, 2
      check fields["a"][0].getInt(), 1'i64
      let elems = getElems(c["a"])
      check elems.len, 3
      check elems[1].getInt(), 2'i64
      check getFields(c, initOrderedTable[string, JsonNode]()).len, 2
      check getElems(c["a"], @[]).len, 3

      var ks: seq[string] = @[]
      for k in keys(c):
        ks.add k
      check ks.len, 2
      check ks[0], "a"
      check ks[1], "b"

      check $c, """{"a":[1,2,3],"b":"hi"}"""
      var ugly = "prefix:"
      toUgly(ugly, c)
      check ugly, "prefix:" & $c
      check pretty(c), """{
  "a": [
    1,
    2,
    3
  ],
  "b": "hi"
}"""
      check escapeJsonUnquoted("\n"), "\\n"
      check escapeJsonUnquoted("\0"), "\\u0000"
      check escapeJsonUnquoted("\v"), "\\u000b"
      check escapeJsonUnquoted("\14"), "\\u000E"
      check escapeJson("a"), "\"a\""
      check escapeJsonUnquoted("\""), "\\\""
      check escapeJsonUnquoted("\\"), "\\\\"

      var nested = parseJson("""{"x":{"y":7}}""")
      let nc = nested.root
      check nc{"x", "y"}.getInt(), 7'i64
      check nc{"x", "missing"}.kind, JNull
      check nc{0}.kind, JNull

      var empty = parseJson("""{"a":[],"b":{}}""")
      check pretty(empty.root), """{
  "a": [],
  "b": {}
}"""

      var a = parseJson("""{"x":1,"y":[2,3]}""")
      var b = parseJson("""{"y":[2,3],"x":1}""")
      check a.root == b.root, true
      check hash(a.root) == hash(b.root), true
      check a.root == c, false
    except:
      check false, true

  block round_trip:
    var t1 = parseJson("""{"a":[1,2,3],"b":"hi","c":{"x":1.5}}""")
    let s = $t1
    var t2 = parseJson(s)
    check $t2, s

  block line_info:
    var t = parseJson("{\n  \"a\": 1\n}", "doc.json", KeepLineInfo)
    let c = t{"a"}
    check info(c).line, 2'i32
    check fileName(c), "doc.json"

  block typed_round_trip:
    let p = Point(x: 3, y: 4, label: "hi", shade: green)
    var tree = toJson(p)
    check $tree, """{"x":3,"y":4,"label":"hi","shade":"green"}"""
    var back = fromJson(tree, Point)
    check back.x, 3
    check back.label, "hi"
    check back.shade, green

  block typed_with_discriminator:
    var tree = toJson(Box(v: 7), JsonOptions(typeFieldName: "type"))
    check $tree, """{"type":"Box","v":7}"""

  block error_as_null:
    # A malformed value becomes (null "msg"): still a JNull, still prints null,
    # but the diagnostic is recoverable.
    var t = parseJson("xyz")
    check kind(t.root), JNull       # reads as null to anything not looking
    check $t, "null"
    check isError(t.root), true     # ... but the error is recoverable
    check hasError(t), true
    check getStr(t.root), ""        # scalar readers degrade to defaults

  block clean_null_is_not_error:
    var t = parseJson("null")
    check kind(t.root), JNull
    check isError(t.root), false
    check hasError(t), false
    check errorMsg(t), ""

  block error_inside_array:
    var t = parseJson("[1, nul, 3]")
    check kind(t.root), JArray
    check len(t.root), 3
    check $t, "[1,null,3]"          # truncation-free print, errors look like null
    check hasError(t), true         # ... yet detectable
    var c = t.root
    var seen = 0
    for el in items(c):
      if isError(el): inc seen
    check seen, 1

  block out_of_range_int:
    # An integer literal beyond int64 (e.g. C's UINT64_MAX in header dumps) used
    # to abort the whole parse; now it falls back to a float atom, JSON-style,
    # and the surrounding integers still parse.
    var t = parseJson("[1, 18446744073709551615, 3]")
    check kind(t.root), JArray
    check len(t.root), 3
    var c = t.root
    var ints: seq[int64] = @[]
    var floats = 0
    for el in items(c):
      var m = el
      if kind(m) == JInt: ints.add getInt(m)
      elif kind(m) == JFloat: inc floats
    check ints.len, 2
    check ints[0], 1'i64
    check ints[1], 3'i64
    check floats, 1

  block error_inside_object:
    var t = parseJson("""{"a": 1, "b": nope}""")
    check kind(t.root), JObject
    check hasError(t), true
    check isError(t{"b"}), true
    check isError(t{"a"}), false

  block truncated_object:
    var t = parseJson("""{"a": 1""")  # missing closing brace
    check kind(t.root), JObject
    check hasError(t), true

  block malformed_is_never_silent:
    # Input that is not JSON must never parse as a clean (shorter) document.
    for bad in ["""{"a": 1 "b": 2}""", "[1 2]", """[{"a": 1 "b": 2}, 3]""",
                "[1] [2]", "{} x", "1 2", """{"a" 1}""", """{"a": 1, 2: 3}""",
                "[1, 2"]:
      var t = parseJson(bad)
      if not hasError(t): echo "accepted silently: ", bad
    for good in ["{}", "[]", """{"a": [1, {"b": null}], "c": "d"}""", " 1 "]:
      var t = parseJson(good)
      if hasError(t): echo "rejected: ", good, " ", errorMsg(t)

  block missing_key:
    # `{}` on an absent key (or a non-object root) used to hand back a nil
    # cursor that crashed the first reader; now it is a real `null`.
    var t = parseJson("""{"a": 1}""")
    check kind(t{"nope"}), JNull
    check getInt(t{"nope"}, 7), 7'i64
    check isError(t{"nope"}), false
    check len(t{"nope"}), 0
    var arr = parseJson("[1, 2]")
    check kind(arr{"a"}), JNull

  block nesting_limit:
    # Deeply nested input used to overflow the parser's stack.
    var deep = ""
    for i in 0..<100_000: deep.add '['
    var t = parseJson(deep)
    check kind(t.root), JArray
    check hasError(t), true
    var ok = ""
    for i in 0..<JsonMaxDepth: ok.add '['
    for i in 0..<JsonMaxDepth: ok.add ']'
    var t2 = parseJson(ok)
    check hasError(t2), false

  echo "json self-tests passed"

main()
