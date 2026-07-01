import std/[syncio, assertions, strutils]

# Regression for nim-lang/nimony#1985: the init analysis used to reject a
# provably-initialized `result` when all of these combined: a `let` rebind,
# an `if`/`else` whose one branch sets `result` directly, and a `case` inside
# a `while` with a `return` in its `else` branch. The "result is initialized
# whenever the return-flag is false" implication was dropped at the outer
# if/else merge because one branch initialized `result` unconditionally
# (Always) while the other did so only under the return-flag condition, and
# the merge required an exact conditional match. Always now subsumes the
# conditional, so the fact survives and the function type-checks.

type Kind = enum kA, kB, kC

func classify(s: string): Kind =
  let t = s
  if t.len == 0:
    result = kC
  else:
    var i = 0
    while i < t.len:
      case t[i]
      of '0'..'9':
        inc i
      else:
        return kC          # leaving path: result set, return-flag true
    result = kA            # normal loop exit: result set, return-flag false

assert classify("") == kC
assert classify("123") == kA
assert classify("12x") == kC
assert classify("0") == kA

echo "init-through-case-while: OK"
