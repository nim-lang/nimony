import std/[syncio, assertions]
import deps/msumtype_generic

# Explicit type annotation with imported generic sum type:
let a: Option[int] = Some(val: 42)
assert a.val == 42

let c: Option[int] = None()

# Cross-module type inference from field values:
let d = Some(val: 99)
assert d.val == 99

let e = Some(val: "hello")
assert e.val == "hello"

# Explicit type on constructor:
let b = Option[int](Some(val: 10))
assert b.val == 10

# Either needs explicit annotation since one branch can't infer the other param:
let f: Either[int, string] = Left(a: 42)
let g: Either[int, string] = Right(b: "world")
assert f.a == 42
assert g.b == "world"

# Generic object constructor inference (non-sum type):
let p = Pair(first: 1, second: 2)
assert p.first == 1
assert p.second == 2

let q = Pair(first: "x", second: "y")
assert q.first == "x"
assert q.second == "y"

# Pattern matching in generic procs (cross-module):
assert getOrDefault(a, 0) == 42
assert getOrDefault(c, 99) == 99
assert getOrDefault(d, 0) == 99

echo "tsumtype_generic: OK"
