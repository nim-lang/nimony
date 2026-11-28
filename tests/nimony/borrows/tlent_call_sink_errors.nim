# A `lent` call result borrows its first argument until it is read, so that
# location cannot be passed to a `sink` parameter in between, not even when the
# call that reads the borrow comes first (planetis-m/nimony-bugs-fuzzers#114).

type
  R = object
    id: int

proc `=destroy`(x: R) = discard
proc `=wasMoved`(x: var R) = x.id = 0
proc takeLent(x: var R): lent R = x
proc keep(x: sink R): R = x
proc comb(a, b: R): int = a.id + b.id
proc combSink(a: R; b: sink R): int = a.id + b.id
proc sinkFirst(a: sink R; b: R): int = a.id + b.id
proc pair(a: int; b: sink R): int = a + b.id

proc main =
  var x = R(id: 1)
  discard comb(takeLent(x), keep(x))
  discard combSink(takeLent(x), x)
  discard sinkFirst(x, takeLent(x))
  # A borrow read before `x` is sunk is done.
  discard pair(takeLent(x).id, x)

main()
