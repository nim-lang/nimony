import ../../../lib/std/broadcast
import std/[syncio, assertions]

proc t1() =
  let a = @[1, 2, 3]
  let b = @[4, 5, 6]

  let c = a +. b
  echo c[1]

  let d = @. (a +. b +. c *. a)
  echo d[0]
  echo d[1]
  echo d[2]
  assert d == @[10, 21, 36]

proc t2() =
  let a = [10, 20, 30]
  let b = [1, 2, 3]
  let c = @. (a -. b)
  assert c == [9, 18, 27]

proc t3() =
  let a = @[1.0, 2.0, 4.0]
  let b = @[2.0, 4.0, 8.0]
  let c = @. (b /. a)
  assert c == @[2.0, 2.0, 2.0]

proc double(x: int): int = x * 2

proc t4() =
  let a = @[1, 2, 3]
  let b = @[4, 5, 6]
  let c = @. (map(a, double) +. b)
  assert c == @[6, 9, 12]

proc t5() =
  let a = @[1, 2, 3]
  let b = @[10, 20, 30]
  let c = @. (a *! 3 +. b)
  assert c == @[13, 26, 39]

  let s = 5
  let d = @. (b -! s)
  assert d == @[5, 15, 25]

  let e = @. (a *! 10)
  assert e == @[10, 20, 30]

  let f = @. (a +! 100)
  assert f == @[101, 102, 103]

proc t6() =
  let x = @[1.0, 2.0, 3.0]
  let y = @[4.0, 5.0, 6.0]
  let z = @[0.5, 1.5, 2.5]
  let res = @. ((x +. y) *. (y -. x) /. (z *! 2.0))
  assert res == @[15.0, 7.0, 5.4]

t1()
t2()
t3()
t4()
t5()
t6()
echo "broadcast ok"