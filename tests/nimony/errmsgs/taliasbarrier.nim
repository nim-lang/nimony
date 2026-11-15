# What is known about a shared variable holds until the next barrier that
# does not name it (doc/language.md, "Aliasing").
{.feature: "assumeSync".}

var gLen = 0
type Digit = range[0..9]

proc use(d: Digit): int = int(d)

proc clobber() = gLen = 1000

proc viaGlobal(): int =
  result = 0
  if gLen >= 0 and gLen < 10:
    clobber()               # a barrier: `gLen` is a global
    result = use(gLen)

proc viaCopy(): int =
  result = 0
  let n = gLen
  if n >= 0 and n < 10:
    clobber()
    result = use(n)         # fine: `n` is owned

proc viaVar(v, w: var int): int =
  result = 0
  if v >= 0 and v < 10:
    w = 1000                # a store to a `var` parameter is a barrier
    result = use(v)

proc viaVarNamed(v: var int): int =
  result = 0
  if v >= 0 and v < 10:
    v = 5                   # fine: the barrier names `v`
    result = use(v)

proc viaAddr(): int =
  var x = 3
  let p = addr x
  result = 0
  if x >= 0 and x < 10:
    clobber()               # `x` has its address taken: shared
    result = use(x)
  discard p

proc ownedLocal(): int =
  var x = 3
  result = 0
  if x >= 0 and x < 10:
    clobber()
    result = use(x)         # fine: nothing but a write to `x` changes it

proc inLoop(v: var int): int =
  result = 0
  if v >= 0 and v < 10:
    for i in 0 ..< 3:
      result = result + use(v)
      clobber()             # a barrier in the body is one before each iteration

discard viaGlobal() + viaCopy() + ownedLocal() + viaAddr()
