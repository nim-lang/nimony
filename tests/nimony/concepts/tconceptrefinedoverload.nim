import std/assertions

## An overload constrained by a refined concept (`concept of Base[T]`) is more
## specific than one constrained by its parent, also behind `var`.

type
  Base[T] = concept
    func get(x: Self, i: int): T

  Derived[T] = concept of Base[T]
    func raw(x: Self, i: int): T

  MoreDerived[T] = concept of Derived[T]
    func last(x: Self): T

  Foo = object
  Bar = object

func get(x: Foo, i: int): int = 1
func raw(x: Foo, i: int): int = 2

func get(x: Bar, i: int): int = 1
func raw(x: Bar, i: int): int = 2
func last(x: Bar): int = 3

func pick[T; A: Base[T]](x: var A): string = "Base"
func pick[T; A: Derived[T]](x: var A): string = "Derived"

func pickVal[T; A: Base[T]](x: A): string = "Base"
func pickVal[T; A: Derived[T]](x: A): string = "Derived"

func pick3[T; A: Base[T]](x: A): string = "Base"
func pick3[T; A: Derived[T]](x: A): string = "Derived"
func pick3[T; A: MoreDerived[T]](x: A): string = "MoreDerived"

## A non-generic overload declared first must not hide the generic ones from an
## explicit instantiation `pickMixed[int, Foo](...)`.
type
  Other = object

func pickMixed(a, b: Other): string = "Other"
func pickMixed[T; A: Base[T]](x: var A): string = "Base"
func pickMixed[T; A: Derived[T]](x: var A): string = "Derived"

func viaGeneric[T; A: Derived[T]](x: var A): string = pickMixed[T, A](x)

var f = Foo()
assert pick[int, Foo](f) == "Derived"
assert pick(f) == "Derived"
assert pickVal(f) == "Derived"
assert pick3(f) == "Derived"
assert pick3(Bar()) == "MoreDerived"
assert pickMixed[int, Foo](f) == "Derived"
assert pickMixed(f) == "Derived"
assert pickMixed(Other(), Other()) == "Other"
assert viaGeneric[int, Foo](f) == "Derived"
