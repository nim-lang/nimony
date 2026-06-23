import std/[syncio, assertions]

# Return-type-driven generic inference: a generic routine whose return type
# mentions a typevar that does NOT appear in its parameters can still have that
# typevar bound — from the call's concrete expected type. Here `ok`'s argument
# fixes `T` and the `let`'s annotation fixes `E` (and vice versa for `err`).

type
  Result[T, E] = object
    case
    of Ok: okVal: T
    of Err: errVal: E

proc ok[T, E](v: T): Result[T, E] = result = Ok(okVal: v)
proc err[T, E](e: E): Result[T, E] = result = Err(errVal: e)

proc isOk[T, E](r: Result[T, E]): bool =
  case r
  of Ok(_): result = true
  of Err(_): result = false

let r: Result[int, string] = ok(5)        # E inferred from the target type
let s: Result[int, string] = err("boom")  # T inferred from the target type
assert r.isOk
assert not s.isOk
{.cast(uncheckedAccess).}:
  assert r.okVal == 5
  assert s.errVal == "boom"

echo "treturntype_infer: OK"
