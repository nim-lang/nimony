# A call that parks a CLOBBER-EXPOSED aggregate argument with every callee-saved
# register already bound — the shape that used to be an arkham assertion.
#
# `take` receives two 16-byte aggregates, so each is passed in a GPR PAIR, and
# its last argument is COMPUTED: marshalling that expression clobbers `cl`, one
# of the registers the aggregates were placed in. The backend's answer is to
# park the exposed words elsewhere until every later argument has been
# marshalled. Here there is no callee-saved register to park in — `b1`/`b2` are
# read again by `keep` AFTER the call, so they and the four parameters hold the
# whole file across it — and arkham used to raise:
#
#   arkham x64n: out of registers for a clobber-exposed aggregate call argument
#   in proc work.0 (nothing to spill)
#
# It now falls through to a pool temp and then to memory (`takeParked`). Found
# by `tests/boot`, from a proc in `contracts_fir.nim` that had to be split in
# two to work around it. The reduced Leng twin, with the same oracle, is
# `nativenif/tests/arkham/aggr_arg_parked_exhausted.c.nif`.
import std/syncio

type
  Pair = object
    a, b: uint64

proc take(c: ptr int; lo, hi: Pair; f1, f2, f3: bool) {.noinline.} =
  if f1 and f2 and f3:
    c[] = int(lo.a + hi.a)

proc keep(c: ptr int; a, b: bool) {.noinline.} =
  if a and b: c[] = c[] + 1

proc work(c: ptr int; n: int; flags: uint16; shift: uint64) {.noinline.} =
  let lo = Pair(a: 1'u64, b: 0'u64)
  let hi = Pair(a: 2'u64, b: 0'u64)
  let b1 = n > 0
  let b2 = n > 1
  take(c, lo, hi, b1, b2, (flags shr shift) != 0'u16)
  keep(c, b1, b2)

var q = 1
work(addr q, 2, 0xffff'u16, 11'u64)
write stdout, $q
