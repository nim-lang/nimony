# A value that owns heap memory moves through `channel[T]()`, whose ring never
# leaves the process; it must not reach a `Writer` that may: the bytes of its
# pointers mean nothing anywhere else. The check is `supportsCopyMem(T)` at
# instantiation, so the generic itself compiles.
import std/streamio

let (r, w) = pipe()
let ok = sender[int](w)
let bad = receiver[string](r)
