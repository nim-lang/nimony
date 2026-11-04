type
  D = distinct D
  E = distinct F
  F = distinct E
  O = object
    x: O
  Arr = object
    a: array[2, Arr]
  Tup = object
    t: (int, Tup)
  Variant = object
    case k: bool
    of true: v: Variant
    else: discard
  Base = object of RootObj
    d: Derived
  Derived = object of Base
  Wrap[T] = object
    x: T
  Holder = object
    w: Wrap[Holder]
  Self[T] = object
    s: Self[T]

var s: Self[int]
