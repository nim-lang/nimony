import std/[syncio, assertions]

type
  Foo = object
    a: int = 2
    b: float = 3.0
    c: string = "hi"
    d: int            # no default → zero
  Bar = ref object
    a: int = 2
    b: float = 3.0

block: # object construction expression uses the defaults
  let x = Foo()
  assert x.a == 2 and x.b == 3.0 and x.c == "hi" and x.d == 0

  let y = Bar()
  assert y.a == 2 and y.b == 3.0

block: # `default` uses the defaults, incl. through arrays and tuples
  let x = default(Foo)
  assert x.a == 2 and x.b == 3.0 and x.c == "hi"

  let y = default(array[2, Foo])
  assert y[0].a == 2 and y[1].b == 3.0

  let z = default(tuple[x: Foo])
  assert z.x.a == 2 and z.x.c == "hi"

block: # explicitly set fields win; the rest fall back to defaults
  let x = Foo(a: 99, c: "set")
  assert x.a == 99 and x.b == 3.0 and x.c == "set" and x.d == 0

block: # nested object defaults resolve recursively
  type Outer = object
    inner: Foo
    n: int = 7
  let o = Outer()
  assert o.inner.a == 2 and o.inner.c == "hi" and o.n == 7

block: # generic object: a constant default on a non-generic field still applies
  type Gen[T] = object
    value: T
    tag: int = 5
  let g = Gen[float]()
  assert g.tag == 5 and g.value == 0.0

echo "ok"
