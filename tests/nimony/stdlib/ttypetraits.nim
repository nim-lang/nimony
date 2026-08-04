# `std/typetraits` is implemented entirely by a template plugin — no compiler
# magic. The plugin asks for what it needs: a nominal type arrives as an opaque
# symbol, the plugin answers `needTypes(sym)`, and the compiler re-runs it with
# that declaration attached. That is the `getImpl` a plugin otherwise lacks.

import std / [syncio, typetraits]

type
  MyInt = distinct int
  MyOtherInt = distinct MyInt
  MyFloat = distinct float
  Foo[T] = object
    x: T

block distinctBaseRecursive:
  var a: distinctBase(MyOtherInt)
  a = 12
  echo a

block distinctBaseShallowOnly:
  var b: distinctBaseShallow(MyOtherInt)
  b = MyInt(7)
  echo int(b)

block distinctBaseOnNonDistinct:
  # a non-distinct type must pass through unchanged
  var c: distinctBase(int)
  c = 5
  echo c

block genericHeadReinstantiated:
  type FooFloat = genericHead(Foo[int])[float]
  var a = default(FooFloat)
  a.x = 2.5
  echo a.x

block genericHeadAsTypeDecl:
  type Bar = genericHead(Foo[string])[int]
  var b = default(Bar)
  b.x = 7
  echo b.x

block genericHeadCrossModule:
  # `seq` is declared in another module: the type closure has to follow the
  # symbol across the module boundary
  var s: genericHead(seq[string])[int] = @[]
  s.add 5
  echo s[0]

block stripGenericParamsPassesNonGenericsThrough:
  var c: stripGenericParams(int)
  c = 3
  echo c

block deferredInsideGeneric:
  # `distinctBase(T)` cannot be answered while `T` is a type variable — the
  # compiler answers the plugin's request for `T` with a `(typevar …)`
  # declaration, so the plugin returns `deferExpansion()`. The call survives generic-body sem as
  # `(at distinctBase T)` and is asked again once each instantiation has
  # substituted — here once as `MyInt`, once as `MyFloat`, once as `int`.
  proc unwrap[T](x: T): distinctBase(T) =
    result = distinctBase(T)(x)

  echo unwrap(MyInt(12))
  echo unwrap(MyFloat(2.5))
  echo unwrap(7)
