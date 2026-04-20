# String handling basics
# =====================
#
# Nim's strutils module provides ASCII string operations.
# For Unicode, use the `unicode` module instead.

import std/[syncio, assertions, strutils]

# --- Searching and testing ---

# `find` returns the index of the first match, or -1.
assert "hello world".find('o') == 4
assert "hello world".find("world") == 6
assert "hello world".find('z') == -1

# `contains` is the boolean version of `find`:
assert "hello world".contains("world")
assert 'o' in "hello"

# `startsWith` / `endsWith`:
assert "config.json".endsWith(".json")
assert "config.json".startsWith("config")

# `continuesWith` checks from a given position:
assert "hello world".continuesWith("world", 6)

# --- Case conversion ---

# ASCII-only variants (fast, no allocation for single chars):
assert 'a'.toUpperAscii == 'A'
assert "Hello".toLowerAscii == "hello"
assert "hello".capitalizeAscii == "Hello"

# --- Comparison ---

# `cmpIgnoreCase` returns 0 for equal, useful for sorting:
assert cmpIgnoreCase("Foo", "foo") == 0

# `cmpIgnoreStyle` also ignores underscores — matches Nim's
# identifier rules where `fooBar` == `foo_bar`:
assert cmpIgnoreStyle("foo_bar", "FooBar") == 0

# --- Splitting ---

# `split` with a set of chars treats each as a separator.
# Adjacent separators produce empty strings:
var parts: seq[string] = @[]
for part in split("a,,b,c", {','}):
  parts.add part
assert parts == @["a", "", "b", "c"]

# --- Search and replace ---

assert "aXbXc".replace('X', '_') == "a_b_c"
assert "hello world".replace("world", "nim") == "hello nim"

# --- Stripping whitespace ---

assert "  hello  ".strip == "hello"
assert "  hello  ".strip(trailing = false) == "hello  "

# --- String formatting ---

# `%` interpolates positional arguments. It is marked as `.raises`
# because invalid format strings cause exceptions at runtime:
try:
  assert "$1 is $2" % ["nim", "great"] == "nim is great"
except:
  discard

echo "strutils: OK"
