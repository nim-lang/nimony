import std/[syncio, assertions]

# The pure-int shape of nim-lang/nimony#1985, from the issue thread. Same
# defect as `tinitcasewhile.nim` but with an `if` inside the `while` instead
# of a `case`, so it lowers differently and is worth pinning separately: the
# fact "result is initialized whenever the return-flag is false" has to
# survive the outer if/else merge, where the other branch initializes
# `result` unconditionally.
#
# The rejections this analysis must keep making live in
# `tests/nimony/errmsgs/tinitnotproven.nim`.

proc classify(n: int): int =
  let t = n
  if t == 0:
    result = 1
  else:
    var i = 0
    while i < t:
      if i mod 2 == 0:
        inc i
      else:
        return 2           # leaving path: result set, return-flag true
    result = 3             # normal loop exit: result set, return-flag false

assert classify(0) == 1   # the `then` branch
assert classify(1) == 3   # loop exits normally
assert classify(2) == 2   # loop returns early

echo "init-through-if-while: OK"
