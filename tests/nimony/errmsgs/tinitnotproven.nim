# The negative side of nim-lang/nimony#1985: `result` that genuinely is NOT
# set on every path must still be rejected.
#
# Nothing else in the tree pins a rejection by the init analysis, so widening
# it — which is what fixing a false positive like #1985 amounts to — could
# start accepting these without a single test noticing. Each proc below is one
# element away from a shape the analysis accepts, and the difference is always
# the same one: no assignment on the path where the loop exits normally.
#
# The positives are `tests/nimony/casestmt/tinitcasewhile.nim` and
# `tinitifwhile.nim`.

proc noPostLoopAssignment(n: int): int =
  # `return` covers only the paths that leave early; falling out of the loop
  # reaches the end of the proc with `result` untouched.
  var i = 0
  while i < n:
    if i mod 2 == 0:
      inc i
    else:
      return 2

proc noPostLoopAssignmentInElse(n: int): int =
  # #1985's own shape with the trailing assignment removed: the `then` branch
  # initializes unconditionally, the `else` branch only on its `return` path.
  let t = n
  if t == 0:
    result = 1
  else:
    var i = 0
    while i < t:
      if i mod 2 == 0:
        inc i
      else:
        return 2

proc thenBranchDoesNotAssign(n: int): int =
  # The mirror image: the loop side is fine, the `then` branch is not.
  let t = n
  if t == 0:
    discard
  else:
    var i = 0
    while i < t:
      if i mod 2 == 0:
        inc i
      else:
        return 2
    result = 3

proc onlyOneIfBranch(n: int): int =
  # The plain baseline, with no loop involved at all.
  if n == 0:
    result = 1
