#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements numerically stable floating-point summation.
##
## Naively adding a sequence of floats accumulates rounding error that grows
## with the number of terms. The routines here keep that error small:
##
## * `sumKbn` uses Kahan-Babuska-Neumaier compensated summation (an improved
##   Kahan sum that also handles the case where the running total is smaller in
##   magnitude than the next term).
## * `sumPairs` uses pairwise (cascade) summation, which recursively splits the
##   input so the error grows only logarithmically.
##
## This is the Nimony port; it operates on `openArray[float]`. Generic and
## `float32` variants of Nim 2's `std/sums` are left for a follow-up.

func sumKbn*(x: openArray[float]): float =
  ## Kahan-Babuska-Neumaier compensated summation of `x`.
  runnableExamples:
    doAssert sumKbn([1.0, 2.0, 3.0]) == 6.0
  if x.len == 0: return 0.0
  var sum = x[0]
  var c = 0.0
  for i in 1 ..< x.len:
    let xi = x[i]
    let t = sum + xi
    if abs(sum) >= abs(xi):
      c += (sum - t) + xi
    else:
      c += (xi - t) + sum
    sum = t
  result = sum + c

func sumPairsImpl(x: openArray[float]; lo, n: int): float =
  ## Sums `x[lo ..< lo + n]` by pairwise recursion.
  if n <= 128:
    result = 0.0
    for i in lo ..< lo + n:
      result += x[i]
  else:
    let half = n div 2
    result = sumPairsImpl(x, lo, half) + sumPairsImpl(x, lo + half, n - half)

func sumPairs*(x: openArray[float]): float =
  ## Pairwise (cascade) summation of `x`.
  runnableExamples:
    doAssert sumPairs([1.0, 2.0, 3.0]) == 6.0
  result = sumPairsImpl(x, 0, x.len)
