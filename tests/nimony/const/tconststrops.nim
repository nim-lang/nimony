import std/assertions

const a = "abc"
const b = "def"
const c = a & "ghi" & b
const d = 7 * len(c) + 6
const e = a == "abc"
const f = a == "def"

assert a == "abc"
assert b == "def"
assert c == "abcghidef"
assert d == (7 * len(c) + 6)
assert e
assert not f
