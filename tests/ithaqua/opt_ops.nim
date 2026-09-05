import std/[syncio, options]

proc double(x: int): int = x * 2
proc isPositive(x: int): bool = x > 0
proc halveIfEven(x: int): Option[int] =
  if x mod 2 == 0: some(x div 2) else: none[int]()

let a = some(21)
let b = none[int]()

echo a.isSome, " ", a.isNone           # true false
echo b.isSome, " ", b.isNone           # false true
echo a.get                             # 21
echo b.get(otherwise = -1)             # -1
echo $a                                # some(21)
echo $b                                # none(int) or none() — whatever `$` says, both backends must agree

echo a.map(double).get                 # 42
echo b.map(double).isNone              # true
echo a.filter(isPositive).isSome       # true
echo some(-5).filter(isPositive).isNone

echo a.flatMap(halveIfEven).isNone     # 21 is odd -> none
echo some(10).flatMap(halveIfEven).get # 5
echo flatten(some(some(7))).get        # 7
echo flatten(some(none[int]())).isNone

# option over a string payload
let s = some("ward")
echo s.get, " ", s.get.len
echo option(0).isSome                  # some(0) is still some
