import std/encodings
import std/assertions

template noRaise(x: untyped): untyped {.untyped.} =
  try:
    x
  except:
    assert false

proc openNoRaise(destEncoding = "UTF-8", srcEncoding = "CP1252"): EncodingConverter =
  try:
    result = open(destEncoding, srcEncoding)
  except:
    assert false

var fromGBK = openNoRaise("utf-8", "gbk")
var toGBK = openNoRaise("gbk", "utf-8")

var fromGB2312 = openNoRaise("utf-8", "gb2312")
var toGB2312 = openNoRaise("gb2312", "utf-8")

block:
  let data = "\215\237\186\243\178\187\214\170\204\236\212\218\203\174\163\172\194\250\180\178\208\199\195\206\209\185\208\199\186\211"
  noRaise:
    assert fromGBK.convert(data) == "醉后不知天在水，满床星梦压星河"

block:
  let data = "万两黄金容易得，知心一个也难求"
  noRaise:
    assert toGBK.convert(data) == "\205\242\193\189\187\198\189\240\200\221\210\215\181\195\163\172\214\170\208\196\210\187\184\246\210\178\196\209\199\243"

block:
  let data = "谁怕？一蓑烟雨任平生"
  noRaise:
    assert toGB2312.convert(data) == "\203\173\197\194\163\191\210\187\203\242\209\204\211\234\200\206\198\189\201\250"


when defined(windows):
  # block should_throw_on_unsupported_conversions:
  #   let original = "some string"

  #   assertRaises(EncodingError):
  #     discard convert(original, "utf-8", "utf-32")

  #   assertRaises(EncodingError):
  #     discard convert(original, "utf-8", "unicodeFFFE")

  #   assertRaises(EncodingError):
  #     discard convert(original, "utf-8", "utf-32BE")

  #   assertRaises(EncodingError):
  #     discard convert(original, "unicodeFFFE", "utf-8")

  #   assertRaises(EncodingError):
  #     discard convert(original, "utf-32", "utf-8")

  #   assertRaises(EncodingError):
  #     discard convert(original, "utf-32BE", "utf-8")

  block should_convert_from_utf16_to_utf8:
    let original = "\x42\x04\x35\x04\x41\x04\x42\x04" # utf-16 little endian test string "тест"
    noRaise:
      let result = convert(original, "utf-8", "utf-16")
      assert(result == "\xd1\x82\xd0\xb5\xd1\x81\xd1\x82")

  block should_convert_from_utf16_to_win1251:
    let original = "\x42\x04\x35\x04\x41\x04\x42\x04" # utf-16 little endian test string "тест"
    noRaise:
      let result = convert(original, "windows-1251", "utf-16")
      assert(result == "\xf2\xe5\xf1\xf2")

  block should_convert_from_win1251_to_koi8r:
    let original = "\xf2\xe5\xf1\xf2" # win1251 test string "тест"
    noRaise:
      let result = convert(original, "koi8-r", "windows-1251")
      assert(result == "\xd4\xc5\xd3\xd4")

  block should_convert_from_koi8r_to_win1251:
    let original = "\xd4\xc5\xd3\xd4" # koi8r test string "тест"
    noRaise:
      let result = convert(original, "windows-1251", "koi8-r")
      assert(result == "\xf2\xe5\xf1\xf2")

  block should_convert_from_utf8_to_win1251:
    let original = "\xd1\x82\xd0\xb5\xd1\x81\xd1\x82" # utf-8 test string "тест"
    noRaise:
      let result = convert(original, "windows-1251", "utf-8")
      assert(result == "\xf2\xe5\xf1\xf2")

  block should_convert_from_utf8_to_utf16:
    let original = "\xd1\x82\xd0\xb5\xd1\x81\xd1\x82" # utf-8 test string "тест"
    noRaise:
      let result = convert(original, "utf-16", "utf-8")
      assert(result == "\x42\x04\x35\x04\x41\x04\x42\x04")

  block should_handle_empty_string_for_any_conversion:
    let original = ""
    noRaise:
      var result = convert(original, "utf-16", "utf-8")
      assert(result == "")
      result = convert(original, "utf-8", "utf-16")
      assert(result == "")
      result = convert(original, "windows-1251", "koi8-r")
      assert(result == "")

block:
  var cp1252: string
  var ibm850: string
  let orig = "öäüß"
  noRaise:
    cp1252 = convert(orig, "CP1252", "UTF-8")
    ibm850 = convert(cp1252, "ibm850", "CP1252")
  let current = getCurrentEncoding()
  assert orig == "\195\182\195\164\195\188\195\159"
  assert ibm850 == "\148\132\129\225"
  noRaise:
    assert convert(ibm850, current, "ibm850") == orig


# block: # fixes about #23481
#   assertRaises EncodingError:
#     discard open(destEncoding="this is a invalid enc")
