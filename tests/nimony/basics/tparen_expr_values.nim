# A value in parentheses keeps its value and type when its one-line branches
# are statement lists: `(if c: 1 else: 2)` holds `(stmts 1)`.
import std/syncio

proc f(): int = 7
proc fr(): int {.raises.} = 9
proc id[T](x: T): T = x
proc two(a, b: int): int = a * 10 + b
type K = enum ka, kb

proc run(c: bool; k: int) =
  let x = (if c: 1 else: 2)
  echo x
  let s = (case k
      of 1: "a"
      else: "b")
  echo s
  let t = (try: fr() except: 0)
  echo t
  let b = (block: 5)
  echo b
  (discard)
  let ab = (discard f(); 3)
  echo ab
  var q = 0
  (inc q; inc q)
  echo q
  let n = (if c: (if k == 1: 10 else: 20) else: (case k
      of 1: 30
      else: 40))
  echo n
  echo two((if c: 1 else: 2), (block: 3))
  echo id((if c: "y" else: "n"))
  let fl: float = (if c: 1.5 else: 2)
  echo fl
  let u8: uint8 = (if c: 200 else: 100)
  echo u8
  let sq: seq[int] = (if c: @[] else: @[1])
  echo sq.len
  let e = (if c: ka else: kb)
  echo e == ka
  let w = (when sizeof(int) == 8: "w64" else: "w32")
  echo w
  var m = 0
  let r = (if c: (inc m; m) elif k == 1: 5 else: 6)
  echo r
  echo (if c: "p" else: "q") & (case k
      of 1: "1"
      else: "x")
  let tup = ((if c: 1 else: 2), (if c: "a" else: "b"))
  echo tup[0], tup[1]
  let arr = [(if c: 1 else: 2), (block: 9)]
  echo arr[0], arr[1]

const
  C1 = (when true: 4 else: 5)
  C3 = (block: 6)
  C4 = [(when false: 1 else: 2), (when true: 3 else: 4)]

proc g[T](x: T; c: bool): T = (if c: x else: id(x))
proc h[T](x: T): string {.untyped.} = (if x.len == 0: "zero" else: "nz")

try:
  run(true, 1)
  run(false, 2)
except:
  echo "exc"
echo C1, C3, C4[0], C4[1]
echo g(3, true), g("s", false), "|"
echo h(""), h("a")
