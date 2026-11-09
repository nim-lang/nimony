import std/[assertions, math, stats, syncio]

func close(a, b: float; eps = 1.0e-12): bool =
  abs(a - b) <= eps

proc main() =
  var rs = RunningStat()
  rs.push(@[1.0, 2.0, 1.0, 4.0, 1.0, 4.0, 1.0, 2.0])
  assert rs.n == 8
  assert rs.min == 1.0
  assert rs.max == 4.0
  assert rs.sum == 16.0
  assert close(rs.mean(), 2.0)
  assert close(rs.variance(), 1.5)
  assert close(rs.varianceS(), 12.0 / 7.0)
  assert close(rs.standardDeviation(), sqrt(1.5))
  assert close(rs.standardDeviationS(), sqrt(12.0 / 7.0))
  assert close(rs.skewness(), 0.8164965809277261)
  assert close(rs.skewnessS(), 1.018350154434631)
  assert close(rs.kurtosis(), -1.0)
  assert close(rs.kurtosisS(), -0.7000000000000008)

  var rs1 = RunningStat()
  var rs2 = RunningStat()
  rs1.push(@[1.0, 2.0, 1.0, 4.0])
  rs2.push(@[1.0, 4.0, 1.0, 2.0])
  let rs3 = rs1 + rs2
  assert rs3.n == 8
  assert close(rs3.sum, rs.sum)
  assert close(rs3.variance(), rs.variance())
  assert close(rs3.skewness(), rs.skewness())
  assert close(rs3.kurtosis(), rs.kurtosis())
  rs1 += rs2
  assert close(rs1.sum, rs.sum)
  assert close(rs1.standardDeviation(), rs.standardDeviation())
  assert close(rs1.varianceS(), rs.varianceS())
  assert close(rs1.kurtosisS(), rs.kurtosisS())

  rs1.clear()
  rs1.push(@[1.0, 2.2, 1.4, 4.9])
  assert close(rs1.sum, 9.5)
  assert close(rs1.mean(), 2.375)

  var ints = RunningStat()
  ints.push(3)
  ints.push(@[1, 2, 4])
  assert ints.n == 4
  assert ints.min == 1.0
  assert ints.max == 4.0
  assert close(ints.mean(), 2.5)

  block: # bug #18718
    var rs = RunningStat()
    rs.push(-1.0)
    assert rs.max == -1.0

  block: # empty accumulators
    var rs = RunningStat()
    assert rs.n == 0
    assert rs.varianceS() == 0.0
    assert rs.standardDeviationS() == 0.0

  block: # standalone funcs
    let data = @[1.0, 2.0, 1.0, 4.0, 1.0, 4.0, 1.0, 2.0]
    assert close(mean(data), rs.mean())
    assert close(variance(data), rs.variance())
    assert close(varianceS(data), rs.varianceS())
    assert close(standardDeviation(data), rs.standardDeviation())
    assert close(standardDeviationS(data), rs.standardDeviationS())
    assert close(skewness(data), rs.skewness())
    assert close(skewnessS(data), rs.skewnessS())
    assert close(kurtosis(data), rs.kurtosis())
    assert close(kurtosisS(data), rs.kurtosisS())

    let intData = @[1, 2, 3, 4]
    assert close(mean(intData), 2.5)
    assert close(variance(intData), 1.25)

  block: # RunningRegress
    var rr = RunningRegress()
    rr.push(@[0.0, 1.0, 2.8, 3.0, 4.0], @[0.0, 1.0, 2.3, 3.0, 4.0])
    assert rr.n == 5
    assert close(rr.slope(), 0.9695585996955861)
    assert close(rr.intercept(), -0.03424657534246611)
    assert close(rr.correlation(), 0.9905100362239381)

    var rr1 = RunningRegress()
    var rr2 = RunningRegress()
    rr1.push(@[0.0, 1.0], @[0.0, 1.0])
    rr2.push(@[2.8, 3.0, 4.0], @[2.3, 3.0, 4.0])
    let rr3 = rr1 + rr2
    assert rr3.n == 5
    assert close(rr3.x_stats.sum, rr.x_stats.sum)
    assert close(rr3.y_stats.sum, rr.y_stats.sum)
    assert close(rr3.correlation(), rr.correlation())
    assert close(rr3.slope(), rr.slope())
    assert close(rr3.intercept(), rr.intercept())
    rr1 += rr2
    assert close(rr1.correlation(), rr.correlation())

    rr.clear()
    assert rr.n == 0
    rr.push(1, 3)
    rr.push(2, 5)
    assert rr.n == 2
    assert close(rr.x_stats.mean(), 1.5)

  block: # string representation
    var simple = RunningStat()
    simple.push([0.0, 2.0])
    echo $simple

  echo "stats: ", rs.n, " ", ints.n, " ", close(rs.mean(), 2.0)

main()
