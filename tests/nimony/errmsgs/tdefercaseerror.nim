# A non-exhaustive `case` inside a proc that also carries a `defer`.
#
# `sem` appends its diagnostics as extra children of the node they are about, so
# the checked tree here is `(case k (of …) (of …) (err "not all cases …"))`. The
# defer-to-try/finally transform walks that tree afterwards and used to `break`
# out of the child loop on anything that was not an `of`/`elif`/`else`, leaving
# the remaining children unconsumed — the balance assertion then fired and the
# compiler died with "into: body did not consume all N children" instead of
# printing the diagnostic below.

type K = enum kA, kB, kC

proc main(k: K) =
  defer:
    discard "cleanup"
  case k
  of kA: discard
  of kB: discard

main(kA)
