## Negative test: the `||` plugin's static race-rule checker must reject an
## output write nested inside an inner loop — a single outer iteration `i`
## would then span a whole index range and adjacent chunks would collide.
import std / parfor

const N = 100
var x: array[N, int]

proc main =
  for i in 0 || N:
    for j in 0 ..< 3:
      x[i] = j

main()
