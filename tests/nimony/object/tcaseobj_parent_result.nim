# `=wasMoved` of a result whose ancestor is a case object writes the
# inherited selector at its inheritance depth.
import std/syncio

type
  K = enum ka, kb
  CaseBase = object of RootObj
    case k: K
    of ka: s: string
    of kb: n: int
  CaseChild = object of CaseBase
    w: int
  CaseGrand = object of CaseChild
    g: int
  TwoCases = object of CaseBase
    case j: K
    of ka: p: string
    of kb: q: int

proc mkChild(x: string): CaseChild = CaseChild(k: ka, s: x, w: 8)
proc mkGrand(): CaseGrand = CaseGrand(k: kb, n: 7, w: 8, g: 9)
proc mkTwo(): TwoCases = TwoCases(k: kb, n: 7, j: ka, p: "p")

let c = mkChild("s")
echo c.s, c.w
let g = mkGrand()
echo g.n, g.w, g.g
let t = mkTwo()
echo t.n, t.p
