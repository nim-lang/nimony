## A user type that satisfies `Broadcastable` keeps working with the concept
## keyed routines of `std/vectors` in a module that also uses `Vec`, whose
## type plugin triggers a second sem round (nim-lang/nimony#3206).

import std/[vectors, syncio]

type Pair = object
  a, b: float64

func `[]`(p: Pair, i: int): float64 =
  if i == 0: p.a else: p.b

func `[]=`(p: var Pair, i: int, v: float64) =
  if i == 0: p.a = v else: p.b = v

func len(p: Pair): int = 2

func similar(p: Pair): Pair = default(Pair)

func `$`(p: Pair): string = "(" & $p.a & ", " & $p.b & ")"

func boxMin[N: static int, T: SomeFloat](a, b: Vec[N, T]): Vec[N, T] =
  min(a, b)

proc main =
  echo $min(vec2(1.0, 2.0), vec2(3.0, 0.5))
  echo $boxMin(vec2(1.0, 2.0), vec2(3.0, 0.5))
  echo $min(Pair(a: 4.0, b: 0.25), Pair(a: -1.0, b: 9.0))

main()
