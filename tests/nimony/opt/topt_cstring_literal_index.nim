## Regression: a string literal that reaches an INDEXED `cstring` must emit
## its `(NC8*)` cast as one parenthesized unit.
##
## C's postfix operators bind tighter than a cast, so emitting the cast bare
## and letting the subscript emitter append to it produces
## `(NC8*)"lit"[i]`, which C reads as `(NC8*)("lit"[i])` — the i-th char cast
## to a pointer, rather than the i-th char of the string. clang rejects the
## assignment that follows ("incompatible pointer to integer conversion").
##
## Found building a downstream viewer at `-d:release --opt:speed`, where an
## `{.inline.}` helper taking a `cstring` parameter had a literal argument
## substituted into `src[i]`. Only reproduced with inlining on, since without
## it the parameter stays a C variable and no literal is ever subscripted.

import std/assertions

# Direct: a literal converted to cstring and indexed.
let direct = cstring("hello")
assert direct[0] == 'h'
assert direct[4] == 'o'

# Via a const — the literal is substituted at each use site.
const greeting: cstring = "world"
assert greeting[0] == 'w'
assert greeting[4] == 'd'

# The shape that actually caught this: an inline proc whose `cstring`
# parameter is indexed, called with a literal, copying into a fixed buffer.
proc copyInto(dest: var array[8, char], src: cstring) {.inline.} =
  var i = 0
  while i < dest.len - 1 and src[i] != '\0':
    dest[i] = src[i]
    inc i
  dest[i] = '\0'

var buf: array[8, char]
copyInto(buf, "abc")
assert buf[0] == 'a'
assert buf[1] == 'b'
assert buf[2] == 'c'
assert buf[3] == '\0'

# Indexing a literal inside an expression, not just an assignment.
proc firstOf(s: cstring): char {.inline.} = s[0]
assert firstOf("zebra") == 'z'
