## `titer_compose.nim` in its Nim-2 spelling: under `.feature: "autoclosures".`
## an anonymous iterator is a closure iterator implicitly, and `iterator(): T`
## as a TYPE means the closure-iterator type — so issue #2487's example needs
## no annotation anywhere.
##
## The iterator half of the feature goes further than the proc half, which
## deliberately leaves proc *types* alone: `proc(): int` is a real second thing,
## a one-word function pointer, so the annotation distinguishes two inhabited
## types. There is no bare `iterator(): int` value — hexer has exactly two
## itertype lowerings, `.closure.` and `.passive.` — so under the feature the
## unannotated spelling is given the `.closure.` one rather than a type only
## `nil` can inhabit.
{.feature: "autoclosures".}

import std / syncio

func iter*[T](s: seq[T]): iterator(): T =
  return iterator(): T =
    for x in s:
      yield x

proc map*[T, U](it: iterator(): T; f: proc (x: T): U {.closure, noSideEffect.}): iterator(): U =
  return iterator(): U =
    for v in it():
      yield f(v)

proc take*[T](it: iterator(): T; n: int): iterator(): T =
  return iterator(): T =
    var k = 0
    for v in it():
      if k >= n: break
      yield v
      inc k

## The proc TYPE still spells `.closure.` out — that half of the feature is
## unchanged, and the `func` literal below is marked implicitly because it is
## nested, so the formal has to be able to store a closure.
##
## `.passive.` still has to be spelled out too: the feature picks the
## `.closure.` reading for a bare itertype, it does not make the two
## interchangeable.
func cycle*[T](s: seq[T]): iterator(): T =
  return iterator(): T =
    while true:
      for value in iter(s)():
        yield value

proc main() =
  echo "-- pipeline"
  let doubled = map(iter(@[1, 2, 3, 4]), func (v: int): string = "v=" & $(v * 2))
  for x in take(doubled, 3)():
    echo x

  echo "-- cycle"
  var n = 0
  for x in cycle(@[1, 2])():
    echo x
    inc n
    if n >= 5: break

main()
