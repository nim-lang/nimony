import std/[assertions, memfiles]


when not defined(windows):
  # TODO: enable it on windows
  block:
    try:
      var f = memfiles.open("tests/nimony/stdlib/file_for_reading_test.txt")
      let s = cast[cstring](f.mem).borrowCStringUnsafe(f.size)
      assert s == "Test text\n"
      f.close
    except:
      assert false
