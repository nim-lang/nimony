
import std / [syncio, assertions]

proc myop(a, b: string): array[1, string] = [a & ";" & b]

proc myop2(a, b: string): string = a & ":" & b

const
  MyConst = myop("Hello", "World")
  MyConst2 = myop2("Hello", "World")

echo MyConst[0]
echo MyConst2

proc cantusesomething(): int =
  echo "hi"
  42

const s = cantusesomething()
assert s == 42

proc whileLoop(): int =
  var i = 0
  while i < 1:
    inc i
  42

const s2 = whileLoop()
assert s2 == 42

proc forLoop(limit: int): int =
  result = 0
  for i in 1..limit:
    result += i

const s3 = forLoop(3)
assert s3 == 6
