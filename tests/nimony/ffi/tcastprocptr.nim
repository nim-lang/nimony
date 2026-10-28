# #2770: a non-closure proc is castable from and to `pointer` (the `dlsym` idiom)
import std/syncio

proc twice(x: cint): cint {.cdecl.} = x * 2
proc thrice(x: int): int {.nimcall.} = x * 3

let p = cast[pointer](twice)
let f = cast[proc (x: cint): cint {.cdecl.}](p)
echo f(21)

let q = cast[pointer](thrice)
let g = cast[proc (x: int): int {.nimcall.}](q)
echo g(14)
