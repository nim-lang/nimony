import std/math

type
  FloatNumber = concept of FloatArithmetic, HasSqrt

  Printable = concept of Arithmetic, Stringable

type 
  Vector*[N: static[int], T: Arithmetic] = object
    components*: array[N, T]
  Vector2*[T: Arithmetic] = Vector[2, T]
  Vector3*[T: Arithmetic] = Vector[3, T]
  Vector4*[T: Arithmetic] = Vector[4, T]
  
template `[]`*[N: static[int], T: Arithmetic](v: Vector[N, T], index: int): T =
  v.components[index]

proc `[]=`*[N: static[int] ,T: Arithmetic](v: var Vector[N, T], index: int, value: T) {.inline.} =
  v.components[index] = value

template x*[N: static[int], T: Arithmetic](v: Vector[N, T]): T =
  v[0]

proc `x=`*[N: static[int], T: Arithmetic](v: var Vector[N, T], value: T) {.inline.} =
  v[0] = value

template y*[N: static[int], T: Arithmetic](v: Vector[N, T]): T =
  v[1]

proc `y=`*[N: static[int], T: Arithmetic](v: var Vector[N, T], value: T) {.inline.} =
  v[1] = value

template z*[N: static[int], T: Arithmetic](v: Vector[N, T]): T =
  v[2]

proc `z=`*[N: static[int], T: Arithmetic](v: var Vector[N, T], value: T) {.inline.} =
  v[2] = value

template w*[N: static[int], T: Arithmetic](v: Vector[N, T]): T =
  v[3]
  
proc `w=`*[N: static[int], T: Arithmetic](v: var Vector[N, T], value: T) {.inline.} =
  v[3] = value

## equality
proc `==`*[N: static[int], T: Arithmetic](v1, v2: Vector[N, T]): bool =
  for i in 0 ..< N:
    if v1[i] != v2[i]:
      return false
  true

## negation
proc `-`*[N: static[int], T: SignedArithmetic](v: Vector[N, T]): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = -v[i]

## addition
proc `+`*[N: static[int], T: Arithmetic](v1, v2: Vector[N, T]): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = v1[i] + v2[i]

proc `+=`*[N: static[int], T: Arithmetic](v1: var Vector[N, T], v2: Vector[N, T]) =
  for i in 0 ..< N:
    v1[i] = v1[i] + v2[i]

## subtraction
proc `-`*[N: static[int], T: Arithmetic](v1, v2: Vector[N, T]): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = v1[i] - v2[i]

proc `-=`*[N: static[int], T: Arithmetic](v1: var Vector[N, T], v2: Vector[N, T]) =
  for i in 0 ..< N:
    v1[i] = v1[i] - v2[i]

## multiplication
proc `*`*[N: static[int], T: Arithmetic](v1, v2: Vector[N, T]): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = v1[i] * v2[i]

proc `*=`*[N: static[int], T: Arithmetic](v1: var Vector[N, T], v2: Vector[N, T]) =
  for i in 0 ..< N:
    v1[i] = v1[i] * v2[i]

proc `*`*[N: static[int], T: Arithmetic](v: Vector[N, T], s: T): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = v[i] * s

proc `*`*[N: static[int], T: Arithmetic](s: T, v: Vector[N, T]): Vector[N, T] {.inline.} =
  v * s

proc `*=`*[N: static[int], T: Arithmetic](v: var Vector[N, T], s: T) =
  for i in 0 ..< N:
    v[i] = v[i] * s

## division
proc `/`*[N: static[int], T: FloatNumber](v1, v2: Vector[N, T]): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = v1[i] / v2[i]

proc `/=`*[N: static[int], T: FloatNumber](v1: var Vector[N, T], v2: Vector[N, T]) =
  for i in 0 ..< N:
    v1[i] = v1[i] / v2[i]

proc `/`*[N: static[int], T: FloatNumber](v: Vector[N, T], s: T): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = v[i] / s

proc `/`*[N: static[int], T: FloatNumber](s: T, v: Vector[N, T]): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = s / v[i]

proc `/=`*[N: static[int], T: FloatNumber](v: var Vector[N, T], s: T) =
  for i in 0 ..< N:
    v[i] = v[i] / s

proc `div`*[N: static[int], T: IntegerArithmetic](v1, v2: Vector[N, T]): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = v1[i] div v2[i]

proc `div`*[N: static[int], T: IntegerArithmetic](v: Vector[N, T], s: T): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = v[i] div s

proc `div`*[N: static[int], T: IntegerArithmetic](s: T, v: Vector[N, T]): Vector[N, T] =
  result = Vector[N, T]()
  for i in 0 ..< N:
    result[i] = s div v[i]

proc `$`*[N: static[int], T: Printable](v: Vector[N, T]): string =
  result = "["
  for i in 0 ..< N:
    if i > 0:
      result.add(", ")
    result.add($v[i])
  result.add("]")

## Pythagorean theorem
proc length*[N: static[int], T: FloatNumber](v: Vector[N, T]): T =
  result = T(0)
  for i in 0 ..< N:
    result = result + v[i] * v[i]
  result = sqrt(result)

proc length*[N: static[int], T: IntegerArithmetic](v: Vector[N, T]): float =
  result = float(0)
  for i in 0 ..< N:
    result = result + float(v[i]) * float(v[i])
  result = sqrt(result)

## Length without sqrt
proc lengthSquared*[N: static[int], T: Arithmetic](v: Vector[N, T]): T =
  result = T(0)
  for i in 0 ..< N:
    result = result + v[i] * v[i]

## Normalizes the vector
proc normalize*[N: static[int], T: FloatNumber](v: var Vector[N, T]) =
  let length = v.length()
  if length != T(0):
    for i in 0 ..< N:
      v[i] = v[i] / length

## Returns a normalized vector
proc normalized*[N: static[int], T: FloatNumber](v: Vector[N, T]): Vector[N, T] =
  let length = v.length()
  var vector = v
  if length != T(0):
    for i in 0 ..< N:
      vector[i] = vector[i] / length
  return vector

## Dot product
proc dot*[N: static[int], T: Arithmetic](v1, v2: Vector[N, T]): T =
  result = T(0)
  for i in 0 ..< N:
    result = result + v1[i] * v2[i]

## Cross product
proc cross*[T: Arithmetic](v1, v2: Vector2[T]): T {.inline.} =
  v1.x * v2.y - v1.y * v2.x

proc cross*[T: Arithmetic](v1, v2: Vector3[T]): Vector3[T] {.inline.} =
  Vector3[T](components: [v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z, v1.x * v2.y - v1.y * v2.x])

## Distance between two vectors
proc distance*[N: static[int], T: FloatNumber](v1, v2: Vector[N, T]): T {.inline.} =
  (v1 - v2).length()

proc distance*[N: static[int], T: IntegerArithmetic](v1, v2: Vector[N, T]): float {.inline.} =
  (v1 - v2).length()

## Distance without sqrt
proc distanceSquared*[N: static[int], T: Arithmetic](v1, v2: Vector[N, T]): T {.inline.} =
  (v1 - v2).lengthSquared()

## A vector with all components equal to zero
proc zero*[N: static[int], T: Arithmetic](_: typedesc[Vector[N, T]]): Vector[N, T] {.inline.} =
  default(Vector[N, T])

## A vector with all components equal to one
proc one*[N: static[int], T: Arithmetic](_: typedesc[Vector[N, T]]): Vector[N, T] =
  result = default(Vector[N, T])
  for i in 0 ..< N:
    result[i] = T(1)
