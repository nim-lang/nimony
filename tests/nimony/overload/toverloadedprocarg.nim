# An overloaded routine passed where a `proc` type is expected resolves to the
# unique overload whose signature matches the parameter type. nim-lang/nimony#1973
import std/syncio

proc combine(a, b: int): int = a * b
proc combine(a, b: string): int = a.len + b.len

proc applyInt(f: proc (a, b: int): int): int = f(6, 7)
proc applyStr(f: proc (a, b: string): int): int = f("ab", "cde")

echo applyInt(combine)   # int overload: 6 * 7
echo applyStr(combine)   # string overload: 2 + 3
