import std/result

type E* = enum foo, bar, baz
template foo*(x: int): E = E(x + 1)
const k = 0
let v* = 100

# `foo(k)` is an enum field's template, not a pattern: `k` binds here.
proc labelOf*[T](e: T): string {.untyped.} =
  case e
  of foo(k): result = "bar"
  else: result = "other"

template labelOfT*(e: E): string =
  var s = ""
  case e
  of foo(k): s = "bar"
  else: s = "other"
  s

proc unwrapK*[T](x: T): T {.untyped.} =
  case ok[T, string](x)
  of Ok(v): result = v + k
  of Err(e): result = x
