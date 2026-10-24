# UTF-8 validation cases: minimal and maximal code points, overlong forms,
# surrogates, out-of-range values, truncated sequences, and error offsets.
import std/[assertions, syncio, unicode]

# These checks also run in danger builds.
template check(condition: bool) =
  if not condition: raiseAssert("UTF-8 validation check failed")

const
  Invalid = ["\xc0\x80", "\xc0\xae", "\xe0\x80\x80", "\xe0\x80\xaf",
    "\xe0\x81\x81", "\xf0\x82\x82\xac", "\xf4\x90\x80\x80",
    "\xed\xa0\x80", "\xed\xbf\xbf"]
  Valid = ["\xc2\x80", "\xdf\xbf", "\xe0\xa0\x80", "\xed\x9f\xbf",
    "\xee\x80\x80", "\xef\xbf\xbf", "\xf0\x90\x80\x80", "\xf4\x8f\xbf\xbf"]

func allAtPosition(samples: openArray[string]; expected: int): bool =
  for sample in samples:
    if validateUtf8(sample) != expected: return false
  result = true

# Exercise compile-time evaluation as well as runtime execution.
const
  ConstValid = allAtPosition(Valid, -1)
  ConstInvalid = allAtPosition(Invalid, 0)
  ConstIncomplete = validateUtf8("abc\xf1\x80")
check ConstValid
check ConstInvalid
check ConstIncomplete == 3

proc checkUtf8(s: string; expected: int) =
  check validateUtf8(s) == expected
  check validateUtf8(toOpenArray(s, 0, s.len - 1)) == expected

block single_sequences:
  for s in Invalid: checkUtf8(s, 0)
  for s in Valid: checkUtf8(s, -1)

block mostly_ascii:
  for i in 32..<64:
    var data = default(array[128, char])
    data[i] = '\xc0'
    check validateUtf8(data) == i
    data[i] = '\xc2'
    check validateUtf8(data) == i

block malformed_sequences:
  for s in ["\xff ", "\x80 ", "\xc1 ", "\xc1", "\xc2", "\xc2 ", "\xc2\xc0",
      "\xe0", "\xe0\x9f", "\xe0\xa0", "\xe0\xa0\xc0", "\xe0\xa0 ",
      "\xed\xa0\x80 ", "\xf1", "\xf1\x80", "\xf1\x80\x80", "\xf1 ",
      "\xf1\x80 ", "\xf1\x80\x80 ", "\xf5\x80\x80\x80", "\xff",
      "\x80", "\xc0", "\xc0\x10", "\xe0\x10", "\xe0\xff\x10",
      "\xf0", "\xf0\x10", "\xf0\xff\x10", "\xf0\xff\xff\x10",
      "\xf8\x88\x80\x80\x80", "\xfc\x84\x80\x80\x80\x80"]:
    checkUtf8(s, 0)
    checkUtf8("A\xc3\xa9 " & s, 4)
    checkUtf8("A\xc3\xa9 " & s & "valid\xff", 4)

block strings:
  for s in ["", "hello", "ascii\0text", "\0\x7f", "eé€\xf0\x90\x80\x80",
      "ศไทย中华Việt Nam"]:
    checkUtf8(s, -1)
  checkUtf8("hello\xff", 5)

block boundaries_and_truncation:
  for s in Valid:
    checkUtf8("prefix" & s & " suffix", -1)
    for length in 1..<s.len:
      checkUtf8(s[0..<length], 0)
      checkUtf8("abc" & s[0..<length], 3)
  for s in Invalid:
    checkUtf8("A\xc3\xa9 " & s, 4)

block character_views_and_offsets:
  let chars = ['\xff', '\xf0', '\x90', '\x80', '\x80', '\xff']
  check validateUtf8(toOpenArray(chars, 1, 4)) == -1
  check validateUtf8(toOpenArray(chars, 1, 3)) == 0
  check validateUtf8(toOpenArray(chars, 1, 0)) == -1
  let invalid = ['\xff', 'a', '\xe2', '\x82', '\xac', '\x80', '\xff']
  check validateUtf8(toOpenArray(invalid, 1, 5)) == 4
  check validateUtf8(toOpenArray(invalid, 1, 2)) == 1

block every_one_and_two_byte_input:
  var data = default(array[2, char])
  for first in 0..255:
    data[0] = char(first)
    check validateUtf8(toOpenArray(data, 0, 0)) == (if first < 128: -1 else: 0)
    for second in 0..255:
      data[1] = char(second)
      let expected =
        if first < 128: (if second < 128: -1 else: 1)
        elif first in 0xc2..0xdf and second in 0x80..0xbf: -1
        else: 0
      check validateUtf8(data) == expected

echo "UTF-8 validation passed"
