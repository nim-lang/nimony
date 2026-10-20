# UTF-8 validation cases: minimal and maximal code points, overlong forms,
# surrogates, out-of-range values, truncated sequences, and chunked input.
import std/[assertions, syncio]
import std/[unicode, private/utf8]

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

# The scanner also runs at compile time, so exercise both modes here.
const
  ConstValid = allAtPosition(Valid, -1)
  ConstInvalid = allAtPosition(Invalid, 0)
  ConstIncomplete = validateUtf8("abc\xf1\x80")
check ConstValid
check ConstInvalid
check ConstIncomplete == 3

proc scan(s: string): Utf8Scan =
  scanUtf8(toOpenArray(s, 0, s.len - 1))

block single_sequences:
  for s in Invalid: check scan(s).validUpTo == 0
  for s in Valid: check scan(s).validUpTo == -1

block mostly_ascii:
  for i in 32..<64:
    var data = default(array[128, byte])
    data[i] = 0xC0
    check scanUtf8(data).validUpTo == i
    data[i] = 0xC2
    check scanUtf8(data).validUpTo == i

proc checkError(input: string; errorLen: int) =
  let error = scan("A\xc3\xa9 " & input)
  check error.validUpTo == 4
  check error.errorLen == errorLen

block error_lengths:
  checkError("\xff ", 1)
  checkError("\x80 ", 1)
  checkError("\xc1 ", 1)
  checkError("\xc1", 1)
  checkError("\xc2", 0)
  checkError("\xc2 ", 1)
  checkError("\xc2\xc0", 1)
  checkError("\xe0", 0)
  checkError("\xe0\x9f", 1)
  checkError("\xe0\xa0", 0)
  checkError("\xe0\xa0\xc0", 2)
  checkError("\xe0\xa0 ", 2)
  checkError("\xed\xa0\x80 ", 1)
  checkError("\xf1", 0)
  checkError("\xf1\x80", 0)
  checkError("\xf1\x80\x80", 0)
  checkError("\xf1 ", 1)
  checkError("\xf1\x80 ", 2)
  checkError("\xf1\x80\x80 ", 3)

block strings:
  check scan("hello").validUpTo == -1
  check scan("ศไทย中华Việt Nam").validUpTo == -1
  check scan("hello\xff").validUpTo == 5

block utf8_chars:
  check scan("eé€\xf0\x90\x80\x80").validUpTo == -1
  for s in ["\x80", "\xc0", "\xc0\x10", "\xe0", "\xe0\x10", "\xe0\xff\x10",
      "\xf0", "\xf0\x10", "\xf0\xff\x10", "\xf0\xff\xff\x10"]:
    check scan(s).validUpTo == 0

proc checkChunks(s: string; expected: int) =
  check validateUtf8(s) == expected
  check validateUtf8(toOpenArray(s, 0, s.len - 1)) == expected
  # Every partition into three chunks, including empty chunks and a final
  # chunk that itself contains the error. Positions are relative to all input.
  for first in 0..s.len:
    for second in first..s.len:
      var v = Utf8Validator()
      let a = validateUtf8(v, toOpenArray(s, 0, first - 1))
      check a == -1 or a == expected
      let b = validateUtf8(v, toOpenArray(s, first, second - 1))
      check b == -1 or b == expected
      check validateUtf8(v, toOpenArray(s, second, s.len - 1), final = true) == expected
      # Errors are sticky; a final empty chunk is harmless after success.
      check validateUtf8(v, "", final = true) == expected
      if expected >= 0:
        check validateUtf8(v, "valid text") == expected
      v = default(Utf8Validator)
      check validateUtf8(v, "reset", final = true) == -1

block incremental:
  for s in Valid:
    checkChunks(s, -1)
    checkChunks("prefix" & s & " suffix", -1)
    for length in 1..<s.len:
      checkChunks(s[0..<length], 0)
      checkChunks("abc" & s[0..<length], 3)
  for s in Invalid:
    checkChunks(s, 0)
    checkChunks("A\xc3\xa9 " & s, 4)
  for s in ["\x80", "\xc1", "\xc2 ", "\xe0\x9f", "\xe0\xa0 ",
      "\xf1 ", "\xf1\x80 ", "\xf1\x80\x80 ", "\xf5\x80\x80\x80", "\xff"]:
    checkChunks(s, 0)
    checkChunks("abc" & s, 3)
  for s in ["", "ascii\0text", "eé€\xf0\x90\x80\x80", "ศไทย中华Việt Nam"]:
    checkChunks(s, -1)

block byte_views_and_offsets:
  let bytes = [0x61'u8, 0xF0, 0x90, 0x80, 0x80, 0x62]
  var v = Utf8Validator()
  check validateUtf8(v, toOpenArray(bytes, 1, 2)) == -1
  check validateUtf8(v, toOpenArray(bytes, 3, 4), final = true) == -1
  check validateUtf8(toOpenArray(bytes, 1, 4)) == -1
  v = default(Utf8Validator)
  check validateUtf8(v, "abc\xe2") == -1
  let invalid = [0x80'u8, 0x61]
  check validateUtf8(v, invalid) == 3
  check validateUtf8(v, "", final = true) == 3

block every_one_and_two_byte_input:
  var data = default(array[2, byte])
  for first in 0..255:
    data[0] = byte(first)
    check validateUtf8(toOpenArray(data, 0, 0)) == (if first < 128: -1 else: 0)
    for second in 0..255:
      data[1] = byte(second)
      let expected =
        if first < 128: (if second < 128: -1 else: 1)
        elif first in 0xC2..0xDF and second in 0x80..0xBF: -1
        else: 0
      check validateUtf8(data) == expected
      var v = Utf8Validator()
      discard validateUtf8(v, toOpenArray(data, 0, 0))
      check validateUtf8(v, toOpenArray(data, 1, 1), final = true) == expected

echo "UTF-8 validation passed"
