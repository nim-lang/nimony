# A module that embeds an inherited base object by value defines that base
# (planetis-m/nimony-bugs-llms#207).
import std/syncio
type
  A = ref object of RootObj
    x: int
  B = ref object of A
    y: int
  E = ref object of Exception
    code: int
let b = B(x: 1, y: 2)
let e = E(code: 42)
echo b.x, b.y, " ", e.code
