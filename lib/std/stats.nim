#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Statistical analysis framework for performing
## basic statistical analysis of data.
## The data is analysed in a single pass, when it
## is pushed to a `RunningStat` or `RunningRegress` object.
##
## `RunningStat` calculates for a single data set
## - n (data count)
## - min (smallest value)
## - max (largest value)
## - sum
## - mean
## - variance
## - varianceS (sample variance)
## - standardDeviation
## - standardDeviationS (sample standard deviation)
## - skewness (the third statistical moment)
## - kurtosis (the fourth statistical moment)
##
## `RunningRegress` calculates for two sets of data
## - n (data count)
## - slope
## - intercept
## - correlation
##
## Funcs are provided to calculate statistics on `openArray`s.
##
## However, if more than a single statistical calculation is required, it is more
## efficient to push the data once to a `RunningStat` object and then
## call the numerous statistical funcs for the `RunningStat` object:

runnableExamples:
  from std/math import almostEqual

  template `~=`(a, b: float): bool = almostEqual(a, b)

  var statistics = RunningStat()  # must be var
  statistics.push(@[1.0, 2.0, 1.0, 4.0, 1.0, 4.0, 1.0, 2.0])
  doAssert statistics.n == 8
  doAssert statistics.mean() ~= 2.0
  doAssert statistics.variance() ~= 1.5
  doAssert statistics.varianceS() ~= 1.714285714285715
  doAssert statistics.skewness() ~= 0.8164965809277261
  doAssert statistics.skewnessS() ~= 1.018350154434631
  doAssert statistics.kurtosis() ~= -1.0
  doAssert statistics.kurtosisS() ~= -0.7000000000000008

{.feature: "staticContracts".}

import std/math

type
  RunningStat* = object           ## An accumulator for statistical data.
    n*: int                       ## amount of pushed data
    min*, max*, sum*: float       ## self-explaining
    mom1, mom2, mom3, mom4: float ## statistical moments, mom1 is mean

  RunningRegress* = object ## An accumulator for regression calculations.
    n*: int                ## amount of pushed data
    x_stats*: RunningStat  ## stats for the first set of data
    y_stats*: RunningStat  ## stats for the second set of data
    s_xy: float            ## accumulated data for combined xy

# ----------- RunningStat --------------------------

func clear*(s: out RunningStat) =
  ## Resets `s`.
  s = RunningStat()

func push*(s: var RunningStat; x: float) =
  ## Pushes a value `x` for processing.
  if s.n == 0:
    s.min = x
    s.max = x
  else:
    if s.min > x: s.min = x
    if s.max < x: s.max = x
  inc(s.n)
  # See Knuth TAOCP vol 2, 3rd edition, page 232
  s.sum += x
  let n = float(s.n)
  let delta = x - s.mom1
  let delta_n = delta / float(s.n)
  let delta_n2 = delta_n * delta_n
  let term1 = delta * delta_n * float(s.n - 1)
  s.mom4 += term1 * delta_n2 * (n*n - 3*n + 3) +
              6*delta_n2*s.mom2 - 4*delta_n*s.mom3
  s.mom3 += term1 * delta_n * (n - 2) - 3*delta_n*s.mom2
  s.mom2 += term1
  s.mom1 += delta_n

func push*(s: var RunningStat; x: int) =
  ## Pushes a value `x` for processing.
  ##
  ## `x` is simply converted to `float`
  ## and the other push operation is called.
  s.push(float(x))

func push*[T: float|int](s: var RunningStat; x: openArray[T]) =
  ## Pushes all values of `x` for processing.
  ##
  ## Int values of `x` are simply converted to `float` and
  ## the other push operation is called.
  for val in x:
    s.push(float(val))

func mean*(s: RunningStat): float =
  ## Computes the current mean of `s`.
  s.mom1

func variance*(s: RunningStat): float {.requires: s.n > 0.} =
  ## Computes the current population variance of `s`.
  s.mom2 / float(s.n)

func varianceS*(s: RunningStat): float =
  ## Computes the current sample variance of `s`.
  if s.n > 1: s.mom2 / float(s.n - 1)
  else: 0.0

func standardDeviation*(s: RunningStat): float {.requires: s.n > 0.} =
  ## Computes the current population standard deviation of `s`.
  sqrt(variance(s))

func standardDeviationS*(s: RunningStat): float =
  ## Computes the current sample standard deviation of `s`.
  sqrt(varianceS(s))

func skewness*(s: RunningStat): float {.requires: s.n > 0.} =
  ## Computes the current population skewness of `s`.
  sqrt(float(s.n)) * s.mom3 / pow(s.mom2, 1.5)

func skewnessS*(s: RunningStat): float {.requires: s.n > 2.} =
  ## Computes the current sample skewness of `s`.
  let s2 = skewness(s)
  sqrt(float(s.n*(s.n-1)))*s2 / float(s.n-2)

func kurtosis*(s: RunningStat): float {.requires: s.n > 0.} =
  ## Computes the current population kurtosis of `s`.
  float(s.n) * s.mom4 / (s.mom2 * s.mom2) - 3.0

func kurtosisS*(s: RunningStat): float {.requires: s.n > 3.} =
  ## Computes the current sample kurtosis of `s`.
  float(s.n-1) / float((s.n-2)*(s.n-3)) *
      (float(s.n+1)*kurtosis(s) + 6)

func `+`*(a, b: RunningStat): RunningStat =
  ## Combines two `RunningStat`s.
  ##
  ## Useful when performing parallel analysis of data series
  ## and needing to re-combine parallel result sets.
  let delta = b.mom1 - a.mom1
  let delta2 = delta*delta
  let delta3 = delta*delta2
  let delta4 = delta2*delta2
  let n = float(a.n + b.n)

  let mom1 = (float(a.n)*a.mom1 + float(b.n)*b.mom1) / n
  let mom2 = a.mom2 + b.mom2 + delta2 * float(a.n) * float(b.n) / n
  let mom3 = a.mom3 + b.mom3 +
             delta3 * float(a.n) * float(b.n) * (float(a.n) - float(b.n))/(n*n) +
             3.0*delta * (float(a.n)*b.mom2 - float(b.n)*a.mom2) / n
  let mom4 = a.mom4 + b.mom4 +
             delta4*float(a.n)*float(b.n) * float(a.n*a.n - a.n*b.n + b.n*b.n) /
                 (n*n*n) +
             6.0*delta2 * (float(a.n)*float(a.n)*b.mom2 + float(b.n)*float(b.n)*a.mom2) /
                 (n*n) +
             4.0*delta*(float(a.n)*b.mom3 - float(b.n)*a.mom3) / n

  result = RunningStat(
    n: a.n + b.n,
    min: min(a.min, b.min),
    max: max(a.max, b.max),
    sum: a.sum + b.sum,
    mom1: mom1,
    mom2: mom2,
    mom3: mom3,
    mom4: mom4)

func `+=`*(a: var RunningStat; b: RunningStat) {.inline.} =
  ## Adds the `RunningStat` `b` to `a`.
  a = a + b

func `$`*(a: RunningStat): string {.requires: a.n > 0.} =
  ## Returns a compact string representation of `a`.
  "RunningStat(n: " & $a.n & ", min: " & $a.min & ", max: " & $a.max &
    ", sum: " & $a.sum & ", mean: " & $a.mean &
    ", stddev: " & $a.standardDeviation & ")"

# ---------------------- standalone array/seq stats ---------------------

func mean*[T: float|int](x: openArray[T]): float {.requires: x.len > 0.} =
  ## Computes the mean of `x`.
  var rs = RunningStat()
  rs.push(x)
  rs.mean()

func variance*[T: float|int](x: openArray[T]): float {.requires: x.len > 0.} =
  ## Computes the population variance of `x`.
  var rs = RunningStat()
  rs.push(x)
  {.assume: rs.n > 0.}
  rs.variance()

func varianceS*[T: float|int](x: openArray[T]): float =
  ## Computes the sample variance of `x`.
  var rs = RunningStat()
  rs.push(x)
  rs.varianceS()

func standardDeviation*[T: float|int](x: openArray[T]): float {.
    requires: x.len > 0.} =
  ## Computes the population standard deviation of `x`.
  var rs = RunningStat()
  rs.push(x)
  {.assume: rs.n > 0.}
  rs.standardDeviation()

func standardDeviationS*[T: float|int](x: openArray[T]): float =
  ## Computes the sample standard deviation of `x`.
  var rs = RunningStat()
  rs.push(x)
  rs.standardDeviationS()

func skewness*[T: float|int](x: openArray[T]): float {.requires: x.len > 0.} =
  ## Computes the population skewness of `x`.
  var rs = RunningStat()
  rs.push(x)
  {.assume: rs.n > 0.}
  rs.skewness()

func skewnessS*[T: float|int](x: openArray[T]): float {.requires: x.len > 2.} =
  ## Computes the sample skewness of `x`.
  var rs = RunningStat()
  rs.push(x)
  {.assume: rs.n > 2.}
  rs.skewnessS()

func kurtosis*[T: float|int](x: openArray[T]): float {.requires: x.len > 0.} =
  ## Computes the population kurtosis of `x`.
  var rs = RunningStat()
  rs.push(x)
  {.assume: rs.n > 0.}
  rs.kurtosis()

func kurtosisS*[T: float|int](x: openArray[T]): float {.requires: x.len > 3.} =
  ## Computes the sample kurtosis of `x`.
  var rs = RunningStat()
  rs.push(x)
  {.assume: rs.n > 3.}
  rs.kurtosisS()

# ---------------------- Running Regression -----------------------------

func clear*(r: out RunningRegress) =
  ## Resets `r`.
  r = RunningRegress()

func push*(r: var RunningRegress; x, y: float) =
  ## Pushes two values `x` and `y` for processing.
  r.s_xy += (r.x_stats.mean() - x)*(r.y_stats.mean() - y) *
                float(r.n) / float(r.n + 1)
  r.x_stats.push(x)
  r.y_stats.push(y)
  inc(r.n)

func push*(r: var RunningRegress; x, y: int) {.inline.} =
  ## Pushes two values `x` and `y` for processing.
  ##
  ## `x` and `y` are converted to `float`
  ## and the other push operation is called.
  r.push(float(x), float(y))

func push*[T: float|int](r: var RunningRegress; x, y: openArray[T]) {.
    requires: x.len == y.len.} =
  ## Pushes two sets of values `x` and `y` for processing.
  for i in 0..<x.len:
    r.push(float(x[i]), float(y[i]))

func slope*(r: RunningRegress): float {.requires: r.n > 1.} =
  ## Computes the current slope of `r`.
  let s_xx = r.x_stats.varianceS()*float(r.n - 1)
  r.s_xy / s_xx

func intercept*(r: RunningRegress): float {.requires: r.n > 1.} =
  ## Computes the current intercept of `r`.
  r.y_stats.mean() - r.slope()*r.x_stats.mean()

func correlation*(r: RunningRegress): float {.
    requires: r.n > 1 and r.x_stats.n > 0 and r.y_stats.n > 0.} =
  ## Computes the current correlation of the two data
  ## sets pushed into `r`.
  let t = r.x_stats.standardDeviation() * r.y_stats.standardDeviation()
  r.s_xy / (float(r.n) * t)

func `+`*(a, b: RunningRegress): RunningRegress =
  ## Combines two `RunningRegress` objects.
  ##
  ## Useful when performing parallel analysis of data series
  ## and needing to re-combine parallel result sets.
  let x_stats = a.x_stats + b.x_stats
  let y_stats = a.y_stats + b.y_stats
  let delta_x = b.x_stats.mean() - a.x_stats.mean()
  let delta_y = b.y_stats.mean() - a.y_stats.mean()
  let s_xy = a.s_xy + b.s_xy +
      float(a.n*b.n)*delta_x*delta_y/float(a.n + b.n)

  result = RunningRegress(
    n: a.n + b.n,
    x_stats: x_stats,
    y_stats: y_stats,
    s_xy: s_xy)

func `+=`*(a: var RunningRegress; b: RunningRegress) =
  ## Adds the `RunningRegress` `b` to `a`.
  a = a + b
