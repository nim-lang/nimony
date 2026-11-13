# A method checked outside top level, here in a checked template, used to get
# its error in the routine's body slot, and expanding the template crashed.
type
  A = ref object of RootObj
  B = object of RootObj

template mk() =
  method mm(a: A): int {.base.} = 0
  discard mm(A())
  proc `=destroy`(x: B) = discard # an inheritable type's hook is a method

mk()

proc p() =
  method pm(a: A): int {.base.} = 0
  discard pm(A()) # the declaration stays usable after the error
