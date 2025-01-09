type Foo* = object
  public*: int
  private: int

type Generic*[T] = object
  public*: T
  private: T

proc getPrivate*(x: Foo): int = x.private
proc getPrivate*[T](x: Generic[T]): T = x.private

template getPrivateTempl*(x: Foo): int = x.private
template getPrivateTempl*[T](x: Generic[T]): T = T(x.private)
