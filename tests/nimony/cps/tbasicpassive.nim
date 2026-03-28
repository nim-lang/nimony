import std / syncio

proc io1() {.passive.} =
  echo "io"

proc main1() {.passive.} =
  echo "hello"
  let b = delay io1()

main1()

proc io2(): int {.passive.} =
  echo "io"
  return 0

proc main2() {.passive.} =
  echo "hello"
  echo io2()

main2()

proc d() {.passive.} =
  echo "d"

proc io3() {.passive.} =
  let c = delay()
  echo "afterYield"
  d()

proc main3() {.passive.} =
  echo "hello"
  io3()

main3()

proc main4() =
  io1()
main4()


proc sum(a, b: int): int {.passive.} =
  return a + b

proc main5() {.passive.}=
  var res = sum(1, 2)
  echo "main5: ", res
main5()

proc main6() =
  var res = sum(1, 2)
  echo "main6: ", res
main6()