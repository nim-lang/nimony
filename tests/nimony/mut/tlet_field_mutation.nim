
# issue: mutating a field/element of a `let` object must be rejected

type
  Inner = object
    x: int
  MyObject = object
    field: int
    inner: Inner

let a = MyObject()
a.field = 1        # INVALID: mutates the contents of a `let`

let o = MyObject()
o.inner.x = 2      # INVALID: nested projection of a `let`
