import std/assertions

# A const call whose arguments are all values runs through a program that is
# compiled once per callee signature (`exprexec.executeCall`) and reads the
# argument values at runtime (`std/readnif`). The three `fib` calls share
# one program. Covers every argument shape the reader supports: ordinals,
# enums, floats, strings (with characters NIF must escape), arrays, tuples,
# seqs and plain objects.

type
  Color = enum red, green, blue
  Obj = object
    a: int
    b: string
    c: Color

proc fib(n: int): int =
  if n < 2: n else: fib(n-1) + fib(n-2)

proc describe(o: Obj; f: float; xs: seq[int]): string =
  result = o.b & $o.a & $ord(o.c) & " " & $f & " " & $xs.len

proc pick(t: (string, int); arr: array[3, char]): string =
  result = t[0] & $t[1] & arr[2]

proc next(c: Color; neg: int8; big: uint64): (Color, int, uint64) =
  result = (succ(c), int(neg), big)

const
  f1 = fib(10)
  f2 = fib(15)
  f3 = fib(20)
  d = describe(Obj(a: 3, b: "q\"x)", c: blue), 2.5, @[1, 2, 3])
  p = pick(("abc", -7), ['x', 'y', 'z'])
  s = @["ab", "cd"]
  n = next(red, -128, 18446744073709551615'u64)

assert f1 == 55 and f2 == 610 and f3 == 6765
assert d == "q\"x)32 2.5 3"
assert p == "abc-7z"
assert s.len == 2 and s[1] == "cd"
assert n[0] == green
assert n[1] == -128
assert n[2] == 18446744073709551615'u64
