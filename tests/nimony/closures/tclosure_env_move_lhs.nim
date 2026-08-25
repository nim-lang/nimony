import std/syncio

# Regression (#2357): assigning a closure through a path that itself reads the
# closure environment. The environment is a last read here, so the duplifier
# turns the capture into a move — `let tmp = env; =wasMoved(env)` — and hoists
# it in front of the assignment. The assignment's target `o.inner.cb` is
# reached THROUGH `o`, a captured local living in that very environment, so its
# address had to be taken before the `=wasMoved` emptied it. It was not, and
# the store went through a nil env pointer.

type
  Inner = ref object
    cb: proc (text: string) {.closure.}
  Outer = ref object
    inner: Inner
    tag: string

proc use(o: Outer) = echo o.tag

proc newOuter(): Outer =
  result = Outer(inner: Inner(), tag: "alive")
  let o = result
  o.inner.cb = proc (text: string) {.closure.} =
    o.use()
    echo text

let x = newOuter()
x.inner.cb("hello")
echo "ok"
