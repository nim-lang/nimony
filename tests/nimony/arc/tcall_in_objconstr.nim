## Regression for the self-host UAF surfaced 2026-05-01 by `hastur boot
## --valgrind`. `Outer(inner: makeInner(s))` made xelim lift the call into
## a `let`-temp; the destroyer then injected `=destroy(tmp)` at scope end,
## double-freeing the seq that had already been moved into `result.inner`.
## Fixed at the time by making xelim's call-in-aggregate temp a `cursor`;
## that temp is a `let` again, and the duplifier's move out of it is what
## keeps the aggregate the sole owner (see `taggregate_call_temp`).

type
  Inner = object
    defines: seq[string]

  Outer = object
    inner: Inner

proc makeInner(s: sink string): Inner =
  result = Inner(defines: @[s])

proc makeOuter(s: sink string): Outer =
  Outer(inner: makeInner(s))

import std / syncio

proc main =
  var o = makeOuter("nimony")
  echo o.inner.defines[0]

main()
