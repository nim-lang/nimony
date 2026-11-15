# A custom generic `==` must beat system's structural
# `==[T: tuple|object]`, even though system's overload infers fewer typevars.
import deps/mgeneric_eq_box
import std/syncio

let p = make[int, int]()
let q = make[int, int]()
echo "operator: ", p == q
echo "explicit: ", `==`(p, q)
