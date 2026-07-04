import std/[syncio, assertions, options]

proc dbl(x: int): int = x * 2
proc isEven(x: int): bool = x mod 2 == 0
proc halveIfEven(x: int): Option[int] =
  if x mod 2 == 0: some(x div 2) else: none[int]()

proc main =
  # construction / predicates
  let a = some(5)
  let b = none[int]()
  assert a.isSome
  assert not a.isNone
  assert b.isNone
  assert not b.isSome

  # get / unsafeGet / get-with-default
  assert a.get == 5
  assert a.unsafeGet == 5
  assert a.get(99) == 5
  assert b.get(99) == 99

  # equality
  assert some(1) == some(1)
  assert some(1) != some(2)
  assert none[int]() == none[int]()
  assert some(1) != none[int]()
  assert some("hi") == some("hi")

  # stringify
  assert $some(5) == "Some(5)"
  assert $none[int]() == "None"
  assert $some("x") == "Some(x)"

  # map / filter
  assert a.map(dbl) == some(10)
  assert b.map(dbl) == none[int]()
  assert some(4).filter(isEven) == some(4)
  assert some(3).filter(isEven) == none[int]()
  assert b.filter(isEven) == none[int]()

  # flatMap / flatten
  assert some(8).flatMap(halveIfEven) == some(4)
  assert some(3).flatMap(halveIfEven) == none[int]()
  assert flatten(some(some(7))) == some(7)
  assert flatten(none[Option[int]]()) == none[int]()

  echo "options: OK"

main()
