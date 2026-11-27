# A write to a field of a call's result is lost when the call returns a
# temporary, so it is rejected whatever the call's argument is (#3161).

type
  Counter = object
    n: int
  Box = object
    c: Counter
  CounterRef = ref Counter

proc get(b: Box): Counter = b.c   # returns a copy, not `var Counter`
proc make(): Counter = Counter(n: 1)
proc bump(x: var int) = inc x

# these write into a real location and stay accepted
proc getVar(b: var Box): var Counter = b.c
proc makeRef(): CounterRef = CounterRef(n: 1)
func pair(x: var int): tuple[x: var int] =
  result = (x: x)

proc main() =
  var b = Box(c: Counter(n: 1))
  b.get.n = 5
  inc b.get.n
  bump b.get.n
  make().n = 7

  b.getVar.n = 5
  inc b.getVar.n
  bump b.getVar.n
  makeRef().n = 7
  var i = 0
  pair(i).x = 3

main()
