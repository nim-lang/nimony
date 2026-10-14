import std/syncio

# A `when` over generic parameters as the LAST statement of a routine body:
# the definition cannot tell whether its branches are the result expression or
# statements, so the tail is decided on instantiation.

proc pick[backward: static[bool]](x: int): int =
  when backward: x - 1 else: x + 1
echo pick[true](5), " ", pick[false](5)

proc a[T](x: T): int =
  when T is int: 1 else: 2
proc b[T](x: T): int =
  when T is int: result = 10 else: result = 20
proc c[T](x: T): string =
  when T is int:
    "int"
  elif T is float:
    "float"
  else:
    "other"
proc d[T](x: T) =
  when T is int: echo "d int" else: echo "d other"
proc e[T](x: T): int =
  result = 7
  when T is int: inc result

echo a(1), " ", a("s")
echo b(1), " ", b(1.0)
echo c(1), " ", c(1.0), " ", c("x")
d(1)
d("x")
echo e(1), " ", e("x")
