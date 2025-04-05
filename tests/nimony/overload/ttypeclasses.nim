import std/assertions

# issue #675, shl/shr etc use SomeInteger:
let x = 1'u shl 3'u
assert x == 8
