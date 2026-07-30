{.feature: "untyped".}

type Foo* = object
  public*: int
  private: int

type Generic*[T] = object
  public*: T
  private: T

proc getPrivate*(x: Foo): int =
  let y = Foo(public: x.public, private: x.private)
  result = y.private
proc getPrivate*[T](x: Generic[T]): T =
  let y = Generic[T](public: x.public, private: x.private)
  result = y.private

template getPrivateTempl*(x: Foo): int = x.private
template getPrivateTempl*[T](x: Generic[T]): T = T(x.private)

proc createGeneric*[T](x: int): Generic[T] =
  result = Generic[T](public: T(x), private: T(x))

# A `{.dirty.}` template touching a private field, expanded inside a generic
# of this same module. When that generic is instantiated from another module
# the access is still legitimate: the code was written HERE. Mirrors
# `src/lib/bitabs.nim`'s `getOrInclImpl` / `getOrIncl` pair.
template bumpPrivateImpl*() {.dirty.} =
  x.private = x.private + 1

proc bumpPrivate*[T](x: var Generic[T]) =
  bumpPrivateImpl()

proc readPrivate*[T](x: Generic[T]): T =
  result = x.private
