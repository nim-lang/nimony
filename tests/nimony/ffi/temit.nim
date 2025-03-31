# issue #802

import std/assertions

{.emit: "int x = 123;".}
var cx {.importc: "x", nodecl.}: int32
assert cx == 123

{.emit: """char* strFunc() {
  return "Hello\\World\n";
}
""".}
proc cstrFunc(): cstring {.importc: "strFunc", nodecl.}
assert cstrFunc().borrowCStringUnsafe() == "Hello\\World\n"

{.emit: r"char* str = ""foo\\bar\nbaz"";".}
var cstr {.importc: "str", nodecl.}: cstring
assert cstr.borrowCStringUnsafe == "foo\\bar\nbaz"
