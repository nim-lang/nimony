## Tests for SSO string correctness across short (<= 7), medium (8-14),
## and long (> 14) string sizes.

import std / [syncio, assertions, strutils]

# ---- len ----

proc testLen() =
  var s = ""
  assert s.len == 0
  s = "hello"
  assert s.len == 5
  s = "abcdefg"       # exactly AlwaysAvail (7)
  assert s.len == 7
  s = "abcdefgh"      # medium (8)
  assert s.len == 8
  s = "abcdefghijklmn" # medium max (14)
  assert s.len == 14
  s = "abcdefghijklmno" # long (15)
  assert s.len == 15
  s = "this is a long string here" # long (26)
  assert s.len == 26

testLen()
echo "len ok"

# ---- add char ----

proc testAddChar() =
  var s = ""
  s.add 'a'
  s.add 'b'
  s.add 'c'
  assert s.len == 3
  assert s[0] == 'a'
  assert s[2] == 'c'
  # grow to medium (> 7 chars, <= 14)
  s.add 'd'
  s.add 'e'
  s.add 'f'
  s.add 'g'
  s.add 'h'
  assert s.len == 8
  assert s[7] == 'h'
  # grow to long (> 14 chars)
  s.add 'i'
  s.add 'j'
  s.add 'k'
  s.add 'l'
  s.add 'm'
  s.add 'n'
  s.add 'o'
  assert s.len == 15
  assert s[14] == 'o'

testAddChar()
echo "add char ok"

# ---- add str ----

proc testAddStr() =
  var s = "hello"
  s.add " world"
  assert s.len == 11
  assert s[0] == 'h'
  assert s[10] == 'd'

testAddStr()
echo "add str ok"

# ---- equality ----

proc testEq() =
  # short strings
  assert "hello" == "hello"
  assert not ("hello" == "world")
  # long strings
  assert "this is a long string here" == "this is a long string here"
  assert not ("this is a long string here" == "this is a long string there")

testEq()
echo "equality ok"

# ---- concatenation ----

proc testConcat() =
  # short result
  var c = "foo" & "bar"
  assert c.len == 6
  assert c[0] == 'f'
  assert c[5] == 'r'
  # long result (> 14)
  var z = "abcdefghij" & "klmnopqrstu"
  assert z.len == 21
  assert z[20] == 'u'

testConcat()
echo "concat ok"

# ---- startsWith ----

proc testStartsWith() =
  # empty prefix always matches
  assert "hello".startsWith("")
  assert "".startsWith("")

  # short prefix in longer string
  assert "hello".startsWith("hel")

  # exact short match
  assert "hel".startsWith("hel")

  # short prefix, first char mismatch
  assert not "hello".startsWith("world")

  # s shorter than prefix
  assert not "he".startsWith("hel")

  # medium prefix (8 chars)
  assert "abcdefghij".startsWith("abcdefgh")
  assert not "abcdefghij".startsWith("xbcdefgh")
  assert not "abcdefghij".startsWith("abcdefgx")

  # long s, short prefix (inline cache covers it)
  assert "this is a long string".startsWith("thi")
  assert "this is a long string".startsWith("this is")   # exactly 7 chars

  # long s, medium prefix
  assert "this is a long string".startsWith("this is a")
  assert not "this is a long string".startsWith("this is b")

  # long s, long prefix
  assert "this is a long string".startsWith("this is a long")
  assert not "this is a long string".startsWith("this is a long X")

  # a short prefix still in the heap tier: its `bytes` cache is valid only up
  # to its length, and the bytes above it are stale, not zero
  var shrunk = "ada@example.com"   # heap (15 chars)
  shrunk.setLen(0)
  shrunk.add "name"                # heap, len 4, cache "name" + stale "exa"
  assert "name.0".startsWith(shrunk)
  assert "name".startsWith(shrunk)
  assert not "nam".startsWith(shrunk)
  assert not "nbme.0".startsWith(shrunk)
  var reserved = newStringOfCap(32) # heap from the start, cache never written
  reserved.add "na"
  assert "name".startsWith(reserved)
  assert not "nb".startsWith(reserved)
  # ... and as `s`: the stale bytes must not make a longer prefix match
  assert not shrunk.startsWith("nameexa")
  assert shrunk.startsWith("nam")

testStartsWith()
echo "startsWith ok"

echo "all ok"
