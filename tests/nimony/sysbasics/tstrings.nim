import std/[assertions, strutils]

proc main() =
  discard "abc" == "abc"

main()

var mytext = ""
for i in 0..<10:
  mytext.add "a"

assert mytext.len == 10

block:
  var mytext = newStringOfCap(10)
  assert mytext.len == 0
  mytext.add 'a'
  assert mytext[0] == 'a'
  assert mytext.len == 1
  mytext.add 'b'
  assert mytext == "ab"
  assert mytext.len == 2

# issue 1356
block:
  var s = "foo"
  if s.len > 0:
    s[0] = 'F'
  assert s == "Foo"

  var t = "bar"
  var u = t
  if t.len > 0:
    t[0] = 'c'
  if u.len > 0:
    u[0] = 'd'
  assert t == "car"
  assert u == "dar"

# --- cstring tests ---

# func strlen(a: cstring): csize_t {.importc: "strlen", header: "<string.h>".}
proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

const abc = cstring "abc\n"
const hello = cstring "hello\n"
printf(abc)

let nimHello = fromCString(hello)
assert len(nimHello) == strlen(hello).int
assert cast[cstring](nimHello.rawData) != hello

var s = default(string)
s.add "nim string "
s.add nimHello
printf(s.toCString)

var zerotext = mytext.terminatingZero()
let ctext0 = cast[cstring](zerotext.rawData)
let ctext1 = zerotext.toCString()
# check redundant copy when already has zero terminated
assert ctext0 == ctext1 
assert zerotext.len == strlen(ctext0).int
assert zerotext.len == mytext.len
printf("%s\n", ctext0)

block: # issue #1444
  assert substr("", -1, -1) == ""
  assert substr("", -1, 0) == ""
  assert substr("", -1, 1) == ""
  assert substr("", 0, -1) == ""
  assert substr("", 0, 0) == ""
  assert substr("", 0, 1) == ""
  assert substr("", 1, -1) == ""
  assert substr("", 1, 0) == ""
  assert substr("", 1, 1) == ""

  assert substr("a", -1, -1) == ""
  assert substr("a", -1, 0) == "a"
  assert substr("a", -1, 1) == "a"
  assert substr("a", -1, 2) == "a"
  assert substr("a", 0, -1) == ""
  assert substr("a", 0, 0) == "a"
  assert substr("a", 0, 1) == "a"
  assert substr("a", 0, 2) == "a"
  assert substr("a", 1, -1) == ""
  assert substr("a", 1, 0) == ""
  assert substr("a", 1, 1) == ""
  assert substr("a", 1, 2) == ""
  assert substr("a", 2, -1) == ""
  assert substr("a", 2, 0) == ""
  assert substr("a", 2, 1) == ""
  assert substr("a", 2, 2) == ""

  assert substr("ab", -1, -1) == ""
  assert substr("ab", -1, 0) == "a"
  assert substr("ab", -1, 1) == "ab"
  assert substr("ab", -1, 2) == "ab"
  assert substr("ab", -1, 3) == "ab"
  assert substr("ab", 0, -1) == ""
  assert substr("ab", 0, 0) == "a"
  assert substr("ab", 0, 1) == "ab"
  assert substr("ab", 0, 2) == "ab"
  assert substr("ab", 0, 3) == "ab"
  assert substr("ab", 1, -1) == ""
  assert substr("ab", 1, 0) == ""
  assert substr("ab", 1, 1) == "b"
  assert substr("ab", 1, 2) == "b"
  assert substr("ab", 1, 3) == "b"
  assert substr("ab", 2, -1) == ""
  assert substr("ab", 2, 0) == ""
  assert substr("ab", 2, 1) == ""
  assert substr("ab", 2, 2) == ""
  assert substr("ab", 2, 3) == ""
  assert substr("ab", 3, -1) == ""
  assert substr("ab", 3, 0) == ""
  assert substr("ab", 3, 1) == ""
  assert substr("ab", 3, 2) == ""
  assert substr("ab", 3, 3) == ""

  assert substr("abc", -1, -1) == ""
  assert substr("abc", -1, 0) == "a"
  assert substr("abc", -1, 1) == "ab"
  assert substr("abc", -1, 2) == "abc"
  assert substr("abc", -1, 3) == "abc"
  assert substr("abc", 0, -1) == ""
  assert substr("abc", 0, 0) == "a"
  assert substr("abc", 0, 1) == "ab"
  assert substr("abc", 0, 2) == "abc"
  assert substr("abc", 0, 3) == "abc"
  assert substr("abc", 1, -1) == ""
  assert substr("abc", 1, 0) == ""
  assert substr("abc", 1, 1) == "b"
  assert substr("abc", 1, 2) == "bc"
  assert substr("abc", 1, 3) == "bc"
  assert substr("abc", 2, -1) == ""
  assert substr("abc", 2, 0) == ""
  assert substr("abc", 2, 1) == ""
  assert substr("abc", 2, 2) == "c"
  assert substr("abc", 2, 3) == "c"
  assert substr("abc", 3, -1) == ""
  assert substr("abc", 3, 0) == ""
  assert substr("abc", 3, 1) == ""
  assert substr("abc", 3, 2) == ""
  assert substr("abc", 3, 3) == ""

block:
  var s = "12234"
  var m = prepareMutationAt(s, 1)
  assert m == '2'


block:
  var cstr: cstring = nil

  assert len(cstr) == 0
  var newstr: string = $cstr
  assert newstr == ""
