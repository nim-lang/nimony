## An import bound to THIS module's own name shadows the implicit self-module
## symbol, as it does in Nim. Both used to sit in the same scope under the same
## name, so every qualified use died with "ambiguous identifier" — reported as
## "undeclared identifier: ''". https://github.com/nim-lang/nimony/issues/2308
import std/assertions
import deps/mselfnamealias as tselfnamealias

proc mine(): int = 42

assert tselfnamealias.aliased() == 7
assert mine() == 42
