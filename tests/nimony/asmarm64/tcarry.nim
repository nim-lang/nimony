# `{.assembler.}` multi-word arithmetic on AArch64: loads and stores through
# pointer registers (literal and register indices), an `adds`/`adcs` carry chain
# that runs straight through `mul`/`umulh` (Arm's multiplies leave NZCV alone),
# a borrow chain, branch-free selects with `csel`, the literal 0 as `xzr`, and a
# counted loop closed by `subs` + `zf`. Native-only, like `asmnative`: hastur's
# native suite runs this directory on an AArch64 host, and cross-compiled under
# `qemu-aarch64` on a Linux/x86-64 one.
import std/syncio

type Words = ptr UncheckedArray[uint64]

proc adds(a, b: uint64): uint64 {.instruction: "adds3".}
proc adcs(a, b: uint64): uint64 {.instruction: "adcs3".}
proc subs(a, b: uint64): uint64 {.instruction: "subs3".}
proc sbcs(a, b: uint64): uint64 {.instruction: "sbcs3".}
proc mulLo(a, b: uint64): uint64 {.instruction: "mul3".}
proc mulHi(a, b: uint64): uint64 {.instruction: "umulh".}
proc cselne(a, b: uint64): uint64 {.instruction: "cselne".}
proc csello(a, b: uint64): uint64 {.instruction: "csello".}
proc cmp(a, b: uint64) {.instruction: "cmp".}
proc addi(d: var int; s: int) {.instruction: "add".}
proc subsi(a, b: int): int {.instruction: "subs3".}
proc zf(): bool {.instruction: "zf".}

proc add3(r {.register: "x0".}: Words; a {.register: "x1".}: Words;
          b {.register: "x2".}: Words) {.assembler.} =
  ## r[0..3] = a[0..2] + b[0..2], the carry out in r[3]. A product computed in
  ## the middle of the chain does not disturb it.
  var x {.register: "x3".}: uint64
  var y {.register: "x4".}: uint64
  var p {.register: "x5".}: uint64
  x = a[0]
  y = b[0]
  x = adds(x, y)
  r[0] = x
  x = a[1]
  y = b[1]
  p = mulLo(x, y)
  p = mulHi(x, y)
  x = adcs(x, y)
  r[1] = x
  x = a[2]
  y = b[2]
  x = adcs(x, y)
  r[2] = x
  x = adcs(0, 0)
  r[3] = x

proc sub2(r {.register: "x0".}: Words; a {.register: "x1".}: Words;
          b {.register: "x2".}: Words): uint64 {.assembler.} =
  ## r[0..1] = a - b; the result is 1 if it borrowed (Arm's C is NOT-borrow)
  var x {.register: "x3".}: uint64
  var y {.register: "x4".}: uint64
  var one {.register: "x5".}: uint64
  x = a[0]
  y = b[0]
  x = subs(x, y)
  r[0] = x
  x = a[1]
  y = b[1]
  x = sbcs(x, y)
  r[1] = x
  one = 1
  result = csello(one, 0)

proc mul128(r {.register: "x0".}: Words; a {.register: "x1".}: uint64;
            b {.register: "x2".}: uint64) {.assembler.} =
  var lo {.register: "x3".}: uint64
  var hi {.register: "x4".}: uint64
  lo = mulLo(a, b)
  hi = mulHi(a, b)
  r[0] = lo
  r[1] = hi

proc select(c {.register: "x0".}: uint64; x {.register: "x1".}: uint64;
            y {.register: "x2".}: uint64): uint64 {.assembler.} =
  ## `x` if `c != 0` else `y` — without a branch
  cmp(c, 0)
  result = cselne(x, y)

proc sumWords(p {.register: "x0".}: Words; n {.register: "x1".}: int): uint64 {.assembler.} =
  ## sum of p[0 ..< n], n > 0, through a register index
  var acc {.register: "x2".}: uint64
  var i {.register: "x3".}: int
  var cnt {.register: "x4".}: int
  var w {.register: "x5".}: uint64
  acc = 0
  i = 0
  cnt = n
  while true:
    w = p[i]
    acc = adds(acc, w)
    addi(i, 1)
    cnt = subsi(cnt, 1)
    if zf(): break
  result = acc

proc zeroOut(p {.register: "x0".}: Words) {.assembler.} =
  p[0] = 0
  p[1] = 0

proc keepAcrossIf(a {.register: "x0".}: uint64; b {.register: "x1".}: uint64): uint64 {.assembler.} =
  ## The flags of `cmp(a, b)` survive an `if` whose arms both leave them
  ## alone, so the `csel` after it may still read them.
  var t {.register: "x2".}: uint64
  cmp(a, b)
  if zf():
    t = 1
  else:
    t = 2
  result = cselne(a, t)

proc main =
  var r = [0'u64, 0, 0, 0]
  var a = [0xFFFFFFFFFFFFFFFF'u64, 0xFFFFFFFFFFFFFFFF'u64, 0xFFFFFFFFFFFFFFFF'u64]
  var b = [1'u64, 0, 0]
  add3(cast[Words](addr r), cast[Words](addr a), cast[Words](addr b))
  echo r[0], " ", r[1], " ", r[2], " ", r[3]
  var x = [0'u64, 5'u64]
  var y = [1'u64, 2'u64]
  let bw = sub2(cast[Words](addr r), cast[Words](addr x), cast[Words](addr y))
  echo r[0], " ", r[1], " ", bw
  let bw2 = sub2(cast[Words](addr r), cast[Words](addr y), cast[Words](addr x))
  echo r[0], " ", r[1], " ", bw2
  mul128(cast[Words](addr r), 0xFFFFFFFFFFFFFFFF'u64, 0xFFFFFFFFFFFFFFFF'u64)
  echo r[0], " ", r[1]
  echo select(1, 10, 20), " ", select(0, 10, 20)
  var w = [1'u64, 2, 3, 4, 5]
  echo sumWords(cast[Words](addr w), 5)
  zeroOut(cast[Words](addr w))
  echo w[0], " ", w[1], " ", w[2]
  echo keepAcrossIf(3, 3), " ", keepAcrossIf(3, 4)
main()
