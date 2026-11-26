# A typevar that only a parameter's default mentions cannot be inferred
# from that default when the argument is omitted.
proc p[T: Ordinal; V: Ordinal](a: T; step: V = V(1)): T = a
iterator cd[T: Ordinal; V: Ordinal](a, b: T; step: V = V(1)): T =
  yield a
proc tup[T; V](a: T; step: (V, int) = (V(1), 2)): T = a
proc outer[W: Ordinal](w: W): W = p(w) # fine until instantiated
proc unused[W: Ordinal](w: W): W = p(w)

discard p(3)
for i in cd(3, 0): discard i
discard tup(3)
discard outer(3)
# inferring from a default that does not mention the typevar still works:
proc foo6[T](x: T = 3): T = x
discard foo6()
discard p(3, 1'i8)
