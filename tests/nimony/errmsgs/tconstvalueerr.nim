# An error in a constant expression is reported as itself: the expression is
# not evaluated, so no "cannot evaluate" message wraps it, and it never
# reaches a compile-time sub-compile.

const a = block: zz1
const b = foo(1)
const c: int = block: zz2
const d = (if true: zz3 else: 1)
const e = case 1
  of 1: zz4
  else: 2
const f = try: zz5 except: 1
const g = (discard 0; zz6)
const h = block: (block: zz7)
const i = block: (discard zz8; 1)
when bar(1): discard

proc p =
  const j = block: (let x = 0; zz9)

proc q[T](x: T) =
  const k = block: zz10

template t =
  const m = block: zz11

const n = block: zz12
discard n

p()
q(1)
t()
