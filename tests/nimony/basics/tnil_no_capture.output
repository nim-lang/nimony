
import std/[syncio]

# Regression: a module with NO captured variables anywhere still has to
# rewrite `(nil ClosureProc)` to a `(fn, env)` tuple consistently with its
# surrounding proctype. Before the fix, lambdalifting's pass 2 was skipped
# unless something captured, so the tupconstr value (emitted in pass 1)
# didn't match the un-rewritten proctype and NIFC refused the field-style
# initializer with "field name not in record or union initializer".

proc makeNil(): (nil proc(): int {.closure.}) =
  result = nil

proc takeNil(f: nil proc(): int {.closure.}) =
  echo "ok"

let f = makeNil()
takeNil(f)
takeNil(nil)
