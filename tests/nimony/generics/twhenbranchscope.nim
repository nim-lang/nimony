# In a checked generic, a `when` that depends on a generic parameter is
# checked like an `if`: every branch is its own scope (doc/language.md,
# "Generics").

proc branchLocal[T: SomeFloat](x: T): int =
  # a name used only inside its branch is fine
  when T is float32:
    let b = cast[uint32](x)
    result = int(b shr 31)
  else:
    let b = cast[uint64](x)
    result = int(b shr 63)

proc f[T](x: T) =
  when T is int:
    var y = 1
  else:
    var y = "a"
  discard y

discard branchLocal(1.0'f32)
f(1)
