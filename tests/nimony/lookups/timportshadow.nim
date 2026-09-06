# A local declaration does not HIDE an imported one: overloadable declarations
# (routines and enum fields alike) accumulate into a symbol choice across the
# `import` boundary, and the choice is resolved by context first and by scope
# distance only as a last resort. See "Identifier lookup" in doc/language.md.
import std / [assertions, syncio]
import deps/mimportshadow

# nim-lang/nimony#2454: a local routine must not shadow an imported enum field.
# The expected type is what picks the candidate.
proc red() = discard
assert tint(red) == 1
assert tint(green) == 2

# Same shape, but the collision is with an imported routine: nothing narrows
# the choice here, so scope distance decides and the local one wins.
proc thing(x: int): string = "local"
assert thing(1) == "local"

# A routine value in a proc-typed context is narrowed by that type, and a
# NAMED proc type narrows exactly like the structural one it aliases.
type Fn = proc(x: int): string
let f: Fn = thing
assert f(1) == "local"

# A non-overloadable declaration DOES shadow, imports included.
block:
  let red = 42
  assert red == 42

# An inner enum field shadows nothing, but wins on distance.
block:
  type Color = enum
    red
    green
  let c = red
  assert ord(c) == 0

echo "OK"
