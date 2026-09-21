type Foo = object
  x: int

# The array arrives as a `var` parameter: `array[8, ref Foo]` has no default
# value of its own (a not-nil ref has none), and what this test is about is the
# STORE, not the declaration.
proc fill(a: var array[8, ref Foo]) =
  a[0] = (ref Foo)(x: 3)
  a[1] = nil
