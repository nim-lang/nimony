## Regression: lazy template-body promotion must bind a promoted template's
## own typevars, not homonymous typevars from an enclosing caller.
## Previously produced `type mismatch: got: T but wanted: T` when two
## forward-referencing templates used different concept constraints on `T`
## before the callee's definition.

type
  Add = concept
    func `+`(a, b: Self): Self
    func identity(_: typedesc[Self]): Self
  Plain = concept of Add
  Extra = concept of Add
    func `*`(a, b: Self): Self

template x*[T: Plain](_: typedesc[T]): T = T.two
template y*[T: Extra](_: typedesc[T]): T = T.two
template two*[T: Add](_: typedesc[T]): T = T.identity + T.identity
