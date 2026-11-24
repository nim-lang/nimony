# A `const` in a generic body may depend on a type parameter (#3099): it is
# evaluated per instantiation, like a `when` condition.
import std / [syncio]

func sizeOf[T](): int =
  const n = sizeof(T)
  n

func twice[T](): int =
  const n = sizeof(T)
  const m = n * 2
  when m > 8:
    result = m
  else:
    result = -m

func slots[T](): int =
  const n = sizeof(T) div 2
  var a: array[n, char]
  result = a.len

func scaled[N: static int](x: int): int =
  const k = N * 10
  result = x * k

func notDependent[T](): int =
  const n = sizeof(int16)
  n

echo sizeOf[int]()
echo sizeOf[int8]()
echo sizeOf[int32]()
echo twice[int]()
echo twice[int16]()
echo slots[int64]()
echo scaled[3](2)
echo notDependent[float]()
