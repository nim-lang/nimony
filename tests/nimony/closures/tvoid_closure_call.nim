# issue #2250
# A void-returning closure whose callee is not a plain symbol goes through
# lambdalifting's `(expr (stmts ...) (if env != nil ...))` wrapper, so the `if`
# lands in expression position. xelim must not bind a result temp for it: there
# is no value, and the temp came out as `void x;` in the generated C.
import std/syncio

proc run(a, b: proc() {.closure.}; pick: bool) =
  (if pick: a else: b)()

proc main() =
  var x = 42
  proc first() {.closure.} = echo "x=", x
  proc second() {.closure.} = echo "second"
  run(first, second, true)
  run(first, second, false)

main()
