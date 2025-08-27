import std/assertions
import deps/dir1/ma

assert ma.foo == 123

const foo = 456
assert foo == 456
assert tqualifiedselfmodule.foo == 456
