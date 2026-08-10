# Scalar replacement of an aggregate that is COPIED OUT OF MEMORY, not built by
# a constructor — and of one whose element address is taken through a field.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_sroa_aggregate_load.nim
# Expected (see .output):
#     42 7
#     11
#     3
#
# `var s = t.hashes` — a seq descriptor is `(object (fld :len (i 64)) (fld :data
# (ptr T)))`, and hexer emits one such whole-descriptor copy per element access.
# `runScalarize` used to require the initializer to be an `(oconstr …)`, so a copy
# out of memory was never even a candidate; the copies then reached arkham, which
# homes an aggregate on the stack, and every `s.len` / `s.data` stayed a memory
# round trip. Reading `L.f` once per accessed field at the same program point
# observes exactly the same memory as the whole-object copy did.
#
# The second half is the escape rule: `addr s.data[i]` is the address of an
# element of the array `s.data` POINTS TO, not of `s`'s own storage, so it must
# not disqualify `s` — `s` is merely read there, like any other field load.

import std / [syncio, assertions]

type
  Desc = object
    len: int
    data: ptr UncheckedArray[int]

  Holder = object
    d: Desc
    tag: int

proc sumVia(h: Holder): int =
  # `var loc = h.d` is an aggregate load, not a constructor.
  let loc = h.d
  result = 0
  for i in 0 ..< loc.len:
    result = result + loc.data[i]

proc bumpFirst(h: Holder): int =
  # `addr loc.data[0]` goes through the POINTER `loc.data` — it does not alias
  # `loc` itself, so `loc` stays scalarizable.
  let loc = h.d
  let p = addr loc.data[0]
  p[] = p[] + 1
  result = loc.len

proc main =
  var backing: array[3, int] = [10, 14, 18]
  var h = Holder(d: Desc(len: 3, data: cast[ptr UncheckedArray[int]](addr backing[0])), tag: 7)
  echo sumVia(h), " ", h.tag
  discard bumpFirst(h)
  echo backing[0]
  echo bumpFirst(h)

main()
