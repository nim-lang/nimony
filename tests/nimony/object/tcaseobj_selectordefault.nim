# The branch a constructor fills is the one the discriminator's value selects:
# the value given, the discriminator's declared default, or `default(T)`.
# Compared by value, so a constant, a range or an imported default selects
# the same branch its literal would.
import std/syncio
import deps/mcasedefault

type
  K = enum ka, kb, kc
  Mid = object
    case k: K = kb
    of ka: a: int = 5
    of kb: b: int = 3
    of kc: c: int = 7
  Els = object
    case k: K = kc
    of ka: a: int = 5
    else: e: int = 9
  Str = object
    case k: K = kb
    of ka: a: int = 5
    of kb: s: string = "hello"
    of kc: c: int = 7
  Nest = object
    case k: K = kb
    of ka: a: int = 5
    of kb:
      case k2: bool = true
      of false: f: int = 11
      of true: t: int = 12
    of kc: c: int = 7
  Rng = object
    case r: range[2..9] = 6
    of 2..4: lo: int = 1
    of 5..7: md: int = 2
    else: hi: int = 3
  Ext = object
    case k: KX = DefX
    of xa: a: int = 5
    of xb: b: int = 3
    of xc: c: int = 7
  Rev = object # no default: `default(K)` is `ka`, the last branch
    case k: K
    of kc: c: int = 7
    of kb: b: int = 3
    of ka: a: int = 5
  Bo = object
    case present: bool
    of true: payload: int = 42
    else: none: int = 1
  G[T] = object
    case k: K = kb
    of ka: a: int = 5
    of kb: b: T
    of kc: c: int = 7
  ElsF = object
    case k: K
    of ka: a: int = 4
    else: e: string = "dflt"
  Holder = object
    x: int
    m: Mid

const KB = kb
const KC = kc

proc takeMid(m = Mid()) = echo "arg ", m.k, " ", m.b
proc takeBo(v = default(Bo)) = echo "arg ", v.present, " ", v.none

proc main() =
  let m = Mid(); echo "Mid ", m.k, " ", m.b
  let e = Els(); echo "Els ", e.k, " ", e.e
  let s = Str(); echo "Str ", s.k, " ", s.s
  let ne = Nest(); echo "Nest ", ne.k, " ", ne.k2, " ", ne.t
  let rg = Rng(); echo "Rng ", rg.r, " ", rg.md
  let x = Ext(); echo "Ext ", x.k, " ", x.c
  let rv = Rev(); echo "Rev ", rv.k, " ", rv.a
  let bo = Bo(); echo "Bo ", bo.present, " ", bo.none
  let g = G[int](); echo "G ", g.k, " ", g.b
  let d = default(Str); echo "default ", d.k, " ", d.s
  let h = Holder(); echo "Holder ", h.m.k, " ", h.m.b
  let c3 = Mid(k: KC); echo "KC ", c3.c
  let c4 = Mid(k: KB, b: 4); echo "KB ", c4.b
  let e1 = ElsF(k: KC, e: "const"); echo "else ", e1.e
  takeMid()
  takeBo()

main()
