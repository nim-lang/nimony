import std/syncio

type Foo = concept
    proc one(_: typedesc[Self]): Self
    proc `+`(a, b: Self): Self

# use concept in typedesc template
template one(_: typedesc[int]): int = 1

echo int.one()
echo one(int)

# use typedesc template in other typedesc template
template getTwo[T: Foo](t: typedesc[T]): T =
    t.one() + one(t)

echo int.getTwo()
echo getTwo(int)

# use typedesc template in typedesc function
func getThree[T: Foo](t: typedesc[T]): T =
  t.getTwo() + t.one()

echo int.getThree()
echo getThree(int)

# use typedesc template in function
func addOne[T: Foo](a: T): T =
  a + T.one()

echo 3.addOne()
echo addOne(3)

# must work in inherited concepts as well

type Bar = concept of Foo
  proc `-`(a, b: Self): Self

template getZero[T: Bar](t: typedesc[T]): T =
  t.one() - one(t)

echo int.getZero()
echo getZero(int)
