
import std / [syncio]

proc myop(a, b: string): array[1, string] = [a & ";" & b]

proc myop2(a, b: string): string = a & ":" & b

const
  MyConst = myop("Hello", "World")
  MyConst2 = myop2("Hello", "World")

echo MyConst[0]
echo MyConst2
