when defined(nimony):
  {.feature: "untyped".}   # so the `check` template's `xx != b` resolves to bool

import std/[json, streams, syncio, tables, opt]
import mjson_private

type Color = enum red, green, blue
type Point = object
  x, y: int
  label: string
  shade: Color
type Box = object
  v: int
type Gen[T] = object
  a: T
type Boxed = object
  box: Box
type Tabled = object
  t: Table[string, int]
  ot: OrderedTable[string, int]
type Guide = object             # from Nim's tests/stdlib/tjsonutils.nim
  question: string
  answer: int

template check(a, b) =
  let xx = a
  if xx != b:
    echo "test failed ", astToStr(a), ": got ", xx, " expected ", b
    quit 1

proc failure[T](src: string; tt: typedesc[T]; opts = JsonOptions()): string =
  ## `fromJson`'s error message; "no error" when it did not raise.
  var t = parseJson(src)
  try:
    discard fromJson(t, T, opts)
    result = "no error"
  except ErrorCode:
    result = errorMsg(t)

proc main() {.raises.} =
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

  block control_chars_escaped:
    var t = parseJson("\"\\u0000\\u0001\\b\\u000b\\u001b\\u001f \"")
    check $t, "\"\\u0000\\u0001\\b\\u000b\\u001b\\u001f \""

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
    var shuffled = parseJson("""{"shade":"red","label":"lo","y":2,"x":1}""")
    let q = fromJson(shuffled, Point)    # members match fields by key, not position
    check q.x, 1
    check q.y, 2
    check q.label, "lo"
    check q.shade, red
    var priv = toJson(initSecret(5, "s"))  # private fields of another module
    check $priv, """{"id":5,"hidden":"s"}"""
    var privIn = parseJson("""{"hidden":"t","id":6}""")
    let sec = fromJson(privIn, Secret)
    check sec.id, 6
    check sec.hidden, "t"

  block typed_errors:
    check failure("""[{"v":1},{"v":true}]""", seq[Box]), "expected int, found JBool"
    check failure("""{"x":1,"y":2,"label":"","shade":"pink"}""", Point), "unknown enum value: pink"
    check failure("""{"v":[]}""", seq[Box]), "expected array, found JObject"
    check failure("""{"answer":42}""", Guide), "missing member: question"
    check failure("""{"question":"q","answer":42,"author":"D"}""", Guide), "extra member"
    check failure("""{"box":{"v":"1"}}""", Boxed), "expected int, found JString"
    var short = parseJson("""{"answer":42}""")
    try: discard fromJson(short, Guide)
    except ErrorCode: discard
    check hasError(short), true
    discard fromJson(short, Guide, JsonOptions(allowMissingKeys: true))
    check errorMsg(short), ""            # a read that succeeds clears it

  block typed_allow_keys:       # ported from tjsonutils.nim's testJoptions
    let extra = """{"question":"6*9=?","answer":42,"author":"Douglas Adams"}"""
    var t = parseJson(extra)
    let g = fromJson(t, Guide, JsonOptions(allowExtraKeys: true))
    check g.question, "6*9=?"
    check g.answer, 42
    check failure(extra, Guide, JsonOptions(allowMissingKeys: true)), "extra member"
    var short = parseJson("""{"answer":42}""")
    let h = fromJson(short, Guide, JsonOptions(allowMissingKeys: true))
    check h.question, ""
    check h.answer, 42
    check failure("""{"answer":42}""", Guide, JsonOptions(allowExtraKeys: true)),
          "missing member: question"
    var both = parseJson("""{"answer":42,"author":"Douglas Adams"}""")
    let k = fromJson(both, Guide, JsonOptions(allowExtraKeys: true, allowMissingKeys: true))
    check k.question, ""
    check k.answer, 42

  block typed_with_discriminator:
    var tree = toJson(Box(v: 7), JsonOptions(typeFieldName: "type"))
    check $tree, """{"type":"Box","v":7}"""
    var gen = toJson(Gen[int](a: 1), JsonOptions(typeFieldName: "type"))
    check $gen, """{"type":"Gen","a":1}"""   # without its generic arguments

  block table_hooks:            # ported from jsonutils' Table hooks
    var src = parseJson("""{"t":{"two":2,"one":1},"ot":{"one":1,"three":3}}""")
    let foo = fromJson(src, Tabled)
    check foo.t == [("one", 1), ("two", 2)].toTable, true
    check foo.ot == [("one", 1), ("three", 3)].toOrderedTable, true
    var tree = toJson(Tabled(t: [("two", 2)].toTable, ot: [("one", 1), ("three", 3)].toOrderedTable))
    check $tree, """{"t":{"two":2},"ot":{"one":1,"three":3}}"""

  block option_hooks:           # ported from tjsonutils.nim's testOption
    var a = toJson(some("test"))
    check $a, "\"test\""
    check fromJson(a, Opt[string]).get(""), "test"
    var b = toJson(none[string]())
    check $b, "null"
    check fromJson(b, Opt[string]).isNone, true
    var c = toJson(some(42))
    check $c, "42"
    check fromJson(c, Opt[int]).get(0), 42
    var d = toJson(none[int]())
    check $d, "null"
    check fromJson(d, Opt[int]).isNone, true

  block raw_numbers:
    var small = parseJson("[1,2]", rawIntegers = true)
    var spellings: seq[string] = @[]
    for el in items(small.root):
      check kind(el), JString
      spellings.add getStr(el)
    check spellings.len, 2
    check spellings[0], "1"
    check spellings[1], "2"
    check $small, "[1,2]"

    var big = parseJson("18446744073709551615", rawIntegers = true)
    check kind(big.root), JString
    check getStr(big.root), "18446744073709551615"
    check $big, "18446744073709551615"

    var fl = parseJson("1.2300", rawFloats = true)
    check kind(fl.root), JString
    check getStr(fl.root), "1.2300"
    check $fl, "1.2300"

    var nested = parseJson("""{"a":[1.20,3],"b":true}""",
                           rawIntegers = true, rawFloats = true)
    check $nested, """{"a":[1.20,3],"b":true}"""

    var normal = parseJson("18446744073709551615")
    check kind(normal.root), JFloat

    var line = parseJson("18446744073709551615", "raw.json", KeepLineInfo,
                         nil, rawIntegers = true)
    check fileName(line.root), "raw.json"
    check $line, "18446744073709551615"

    var s = newStringStream("[1.50]")
    var streamTree = parseJson(s, rawFloats = true)
    check $streamTree, "[1.50]"

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

try: main()
except ErrorCode as e: echo "unexpected ", e
