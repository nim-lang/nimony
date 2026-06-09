when defined(nimony):
  {.feature: "untyped".}   # so the `check` template's `xx != b` resolves to bool

import std/[json, syncio]

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

  block int_out_of_range:
    # Integers outside int64 must not abort the parse (libclang/opir emit C's
    # UINT64_MAX etc.); they fall back to a float atom instead.
    var hi = parseJson("[9223372036854775807]")    # int64.high
    for v in items(hi.root): check kind(v), JInt
    var lo = parseJson("[-9223372036854775808]")   # int64.low
    for v in items(lo.root): check kind(v), JInt
    var over = parseJson("[9223372036854775808]")  # int64.high + 1
    for v in items(over.root): check kind(v), JFloat
    var under = parseJson("[-9223372036854775809]")# int64.low - 1
    for v in items(under.root): check kind(v), JFloat
    var umax = parseJson("[18446744073709551615]") # uint64.max
    for v in items(umax.root): check kind(v), JFloat

  echo "json self-tests passed"

main()
