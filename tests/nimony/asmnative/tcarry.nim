# `{.assembler.}` multi-word arithmetic on x86-64: loads and stores through
# pointer registers, an `add`/`adc` carry chain, the full 64x64->128 `mul`, a
# branch-free select with `cmov`, and a counted loop. Native-only: the C backend
# cannot compile an assembler body (see `tests/nimony/assembler/tcbackend.nim`),
# which is why this directory is skipped by the tree walk and run by
# `hastur nativetests` instead.
import std/syncio

type Words = ptr UncheckedArray[uint64]

proc adc(d: var uint64; s: uint64) {.instruction: "adc".}
proc add(d: var uint64; s: uint64) {.instruction: "add".}
proc mul(lo: var uint64; hi: var uint64; s: uint64) {.instruction: "mul".}
proc cmovc(d: var uint64; s: uint64) {.instruction: "cmovc".}
proc cmovnz(d: var uint64; s: uint64) {.instruction: "cmovnz".}
proc cmp(a, b: uint64) {.instruction: "cmp".}
proc test(a, b: uint64) {.instruction: "test".}
proc dec(d: var int) {.instruction: "dec".}
proc test(a, b: int) {.instruction: "test".}
proc zf(): bool {.instruction: "zf".}

proc add2(r {.register: "rdi".}: Words; a {.register: "rsi".}: Words;
          b {.register: "rdx".}: Words) {.assembler.} =
  ## r = a + b over two limbs
  var x {.register: "rax".}: uint64
  var y {.register: "rcx".}: uint64
  x = a[0]
  add(x, b[0])
  r[0] = x
  y = a[1]
  adc(y, b[1])
  r[1] = y

proc mul128(r {.register: "rdi".}: Words; a {.register: "rsi".}: uint64;
            b {.register: "rdx".}: uint64) {.assembler.} =
  ## r[0..1] = a * b
  var lo {.register: "rax".}: uint64
  var bb {.register: "rcx".}: uint64
  bb = b                       # rdx is about to be overwritten with the high half
  var hi {.register: "rdx".}: uint64
  lo = a
  mul(lo, hi, bb)
  r[0] = lo
  r[1] = hi

proc select(c {.register: "rdi".}: uint64; x {.register: "rsi".}: uint64;
            y {.register: "rdx".}: uint64): uint64 {.assembler.} =
  ## `x` if `c != 0` else `y` — without a branch
  var r {.register: "rax".}: uint64
  r = y
  test(c, c)
  cmovnz(r, x)
  result = r

proc sumWords(p {.register: "rdi".}: Words; n {.register: "rsi".}: int): uint64 {.assembler.} =
  ## sum of p[0 ..< n], n > 0, walking the pointer down from the end
  var acc {.register: "rax".}: uint64
  var i {.register: "rsi".}: int
  acc = 0
  while true:
    dec(i)
    add(acc, p[i])
    test(i, i)
    if zf(): break
  result = acc

proc main =
  var r = [0'u64, 0'u64]
  var a = [0xFFFFFFFFFFFFFFFF'u64, 1'u64]
  var b = [1'u64, 2'u64]
  add2(cast[Words](addr r), cast[Words](addr a), cast[Words](addr b))
  echo r[0], " ", r[1]
  mul128(cast[Words](addr r), 0xFFFFFFFFFFFFFFFF'u64, 0xFFFFFFFFFFFFFFFF'u64)
  echo r[0], " ", r[1]
  mul128(cast[Words](addr r), 1'u64 shl 40, 1'u64 shl 40)
  echo r[0], " ", r[1]
  echo select(1, 10, 20), " ", select(0, 10, 20)
  var w = [1'u64, 2, 3, 4, 5]
  echo sumWords(cast[Words](addr w), 5)
main()
