## Concept requirements are checked the way a call is resolved (issue #2275):
## `$` exists for `int64` only, yet `int8` satisfies `Stringable` because the
## call `$(x)` widens — and a `template` may stand in for a `func` requirement
## (`$` for `string` is one).

import std/syncio

type
  Foo = concept of Stringable

func foo[T: Foo](x: T): string = $x
func bar[T: Stringable](x: T): string = $x

echo foo(42'i64)
echo foo(42'i8)
echo bar(42'i8)
echo foo(42'u8)
echo foo("abc")
echo foo(true)
