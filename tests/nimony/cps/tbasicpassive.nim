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
