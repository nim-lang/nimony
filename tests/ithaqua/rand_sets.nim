import std/[syncio, random, intsets, setutils]

# seeded PRNG: same seed → same sequence on both backends
var r = initRand(42)
var draws = ""
var i = 0
while i < 8:
  draws.add $r.rand(999)
  draws.add " "
  i = i + 1
echo draws
var r2 = initRand(42)
echo r2.rand(999)                      # first draw again — reproducible

# (rand(float) was once broken on the NATIVE backend; it has its own fixture now
# — see rand_float.nim)

# intsets: sparse int membership (no len in nimony's IntSet; iteration order
# is hash-dependent — stick to contains)
var s = initIntSet()
s.incl 3
s.incl 1_000_000
s.incl 3
echo s.contains(3), " ", s.contains(4)
s.excl 3
echo s.contains(3), " ", s.contains(1_000_000)
echo containsOrIncl(s, 55), " ", containsOrIncl(s, 55)

# setutils over set[char]
let vowels = {'a', 'e', 'i', 'o', 'u'}
echo 'e' in vowels, " ", 'z' in vowels
let cons = fullSet(char) - vowels
echo 'z' in cons, " ", 'a' in cons
echo card(vowels)                      # 5
