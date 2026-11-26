# a module-qualified ambiguous call names the routine
proc f(a: int32; b: int64): int = 1
proc f(a: int64; b: int32): int = 2
let x = 1'i32
discard tqualified_ambiguous_call.f(x, x)
