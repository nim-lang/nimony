import std/assertions

type
  Number = int32
  Pair = (int, float)
  Nested = ((int, float), string)

proc sizePair[A, B](t: typedesc[(A, B)]): int = sizeof(A) + sizeof(B)
proc sizeNested[A, B, C](t: typedesc[((A, B), C)]): int =
  sizeof(A) + sizeof(B) + sizeof(C)

assert sizePair((int, float)) == sizeof(int) + sizeof(float)
assert sizePair((Number, uint32)) == 8
assert sizePair(((int, float), string)) == sizeof(Pair) + sizeof(string)
assert sizeNested(((int, float), string)) == sizeof(Nested)
assert sizeof((int, float)) == sizeof(Pair)
assert sizeof(((int, float), string)) == sizeof(Nested)
assert sizeof((Number, uint32)) == 8

let values = (17, 23)
let named = (first: 17, second: 23)
assert values[0] == 17 and values[1] == 23
assert named.first == 17 and named.second == 23

# a template's value may be a type expression
template pairType(): untyped = (int, int)
template arrayType(): untyped = array[2, int]
template namedType(): untyped = tuple[a, b: int]
template pairDesc(): typedesc = (int, int8)
template unusedPair(): untyped = (int, float)
assert sizeof(pairType()) == 2 * sizeof(int)
assert sizeof(arrayType()) == 2 * sizeof(int)
assert sizeof(namedType()) == 2 * sizeof(int)
assert sizePair(pairDesc()) == sizeof(int) + 1
