# An `invariant` stands at the start of a loop body and nowhere else.

proc notFirst(): int =
  result = 0
  for i in 0 ..< 3:
    result = 1
    {.invariant: result >= 0.}

proc inIf(): int =
  result = 0
  for i in 0 ..< 3:
    if i == 0:
      {.invariant: result >= 0.}

proc outside(): int =
  result = 0
  {.invariant: result >= 0.}

proc afterAssert(): int =
  result = 0
  while result < 3:
    {.assert: result >= 0, invariant: result >= 0.}
    inc result

discard notFirst()
discard inIf()
discard outside()
discard afterAssert()
