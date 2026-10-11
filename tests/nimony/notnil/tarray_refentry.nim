type Foo = ref object
  x: int

# See `tarray_entry`: the array is a `var` parameter so that the declaration
# does not fail first -- the store is what is under test.
proc fill(a: var array[8, Foo]) =
  a[0] = Foo(x: 3)
  a[1] = nil
