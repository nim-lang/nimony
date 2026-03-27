# Sequences and strings
# =====================
#
# `seq` and `string` are Nim's dynamic arrays. They share
# many operations: `add`, `len`, `high`, `del`, `pop`, `[]`.
# Both use value semantics with copy-on-write for literals.

import std/[syncio, assertions]

# --- Building sequences ---

var s: seq[int] = @[]
s.add 10
s.add 20
s.add 30
assert s == @[10, 20, 30]
assert s.len == 3

# `@` converts an array to a seq:
let primes = @[2, 3, 5, 7, 11]
assert primes[0] == 2
assert primes.high == 4   # highest valid index

# --- Removing elements ---

# `pop` removes and returns the last element:
var stack = @[1, 2, 3]
assert stack.pop == 3
assert stack == @[1, 2]

# `del` swaps with the last element, then shrinks — O(1) but
# does NOT preserve order:
var xs = @[10, 20, 30, 40]
xs.del 1          # removes 20, replaces with 40
assert xs.len == 3
assert 20 notin xs

# --- grow / shrink ---

# `grow` extends a seq to a target length, filling new slots
# with the given value:
var buf = @[1, 2]
buf.grow 5, 0
assert buf.len == 5
assert buf[4] == 0

# `shrink` truncates:
buf.shrink 2
assert buf == @[1, 2]

# --- Strings share the same patterns ---

var greeting = "hello"
greeting.add " world"
assert greeting.len == 11
assert greeting[0] == 'h'

# Iterating over characters:
var vowels = 0
for ch in items(greeting):
  if ch in {'a', 'e', 'i', 'o', 'u'}:
    inc vowels
assert vowels == 3

# --- Value semantics ---

# Assignment copies; mutations are independent:
var a = @[1, 2, 3]
var b = a
b[0] = 99
assert a[0] == 1    # a is unchanged

# Same for strings:
var s1 = "abc"
var s2 = s1
s2[0] = 'X'
assert s1 == "abc"   # s1 is unchanged

# --- concat for efficient string building ---

# `&` allocates a new string each time.
# `concat` is much more efficient for multiple pieces:
let name = "world"
let msg = concat("hello, ", name, "!")
assert msg == "hello, world!"

# --- Bulk writes with beginStore / endStore ---

# When you need to write into a string through a pointer
# (e.g. filling from a C API, copying bytes), use
# beginStore/endStore instead of raw pointer access.
#
# Nim strings use Small String Optimization: long strings
# cache the first bytes inline for fast access. A bulk write
# through the heap pointer makes this cache stale.
# endStore syncs it back. Forgetting it means s[0..6] returns
# wrong data after a write.

var target = newString(10)
let p = beginStore(target, 5)   # get mutable pointer, ensure unique
p[0] = 'H'
p[1] = 'e'
p[2] = 'l'
p[3] = 'l'
p[4] = 'o'
endStore(target)                 # sync inline cache — required!
assert target[0] == 'H'
assert target[4] == 'o'

echo "seqs_and_strings: OK"
