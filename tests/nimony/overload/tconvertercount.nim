# Candidates that tie on every other rank: fewer converted arguments win
# (planetis-m/nimony-bugs-llms#331).
import std/[syncio, unicode]

proc f(s: openArray[char]; xs: openArray[int]): int = 1
proc f(s: string; xs: openArray[int]): int = 2
let s = "ab"
echo f(s, [3, 4])

let t = "  a  "
echo unicode.strip(t, runes = [Rune(0x20)])

type A = object
converter toA(x: int): A = A()
proc k(a: A; b: A): int = 1
proc k(a: A; b: int): int = 2
let x = 1
echo k(x, x)
