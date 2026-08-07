# Tables and hash sets. Hash VALUES differ between the 64-bit native oracle
# and 32-bit wasm, so nothing here may depend on hash order: output goes
# through explicit lookups or a sort before echoing.
import std/[syncio, tables, sets, algorithm]

proc cmpStr(x, y: string): int =
  if x < y: -1 elif x > y: 1 else: 0
proc cmpInt(x, y: int): int = x - y

var t = initTable[string, int]()
t["alpha"] = 1
t["beta"] = 2
t["gamma"] = 3
echo t.len                             # 3
echo t.hasKey("beta"), " ", t.hasKey("delta")
echo t.getOrDefault("gamma", -1)       # 3
echo t.getOrDefault("delta", -1)       # -1
t["beta"] = 20                         # overwrite
echo t.getOrDefault("beta", -1)        # 20
t.del("alpha")
echo t.len, " ", t.contains("alpha")   # 2 false

echo mgetOrPut(t, "delta", 4)          # 4 (inserted)
echo mgetOrPut(t, "delta", 99)         # 4 (already present)

# iteration, made order-independent by sorting the keys
var ks: seq[string] = @[]
for k in t.keys: ks.add k
sort(ks, cmpStr)
for k in ks:
  echo k, "=", t.getOrDefault(k, -1)

# growth across the rehash threshold: 100 int keys, spot-check lookups
var big = initTable[int, int]()
var i = 0
while i < 100:
  big[i] = i * i
  i = i + 1
echo big.len                           # 100
echo big.getOrDefault(0, -1), " ", big.getOrDefault(57, -1), " ",
  big.getOrDefault(99, -1)             # 0 3249 9801

# hash sets
var s = initHashSet[int]()
s.incl 3
s.incl 14
s.incl 3                               # duplicate
echo s.len                             # 2
echo s.contains(14), " ", s.contains(15)
echo containsOrIncl(s, 15)             # false (was missing, now added)
echo containsOrIncl(s, 15)             # true
s.excl 3
echo s.len                             # 2
echo missingOrExcl(s, 999)             # true

var a = initHashSet[int]()
var b = initHashSet[int]()
for x in [1, 2, 3, 4]: a.incl x
for x in [3, 4, 5]: b.incl x
var inter: seq[int] = @[]
for x in intersection(a, b).items: inter.add x
sort(inter, cmpInt)
for x in inter: echo x                 # 3, then 4
