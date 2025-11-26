# issue #1334

proc clear(x: var int) =
  x = 0

proc cannotMutate(x: openArray[int]) =
  for i in 0 ..< x.len:
    x[i] = 1
    clear(x[i])
