import std/syncio

template testTmpl1(procdef: untyped) {.untyped.} =
  let s = astToStr(procdef)
  echo "`" & s & "`"

proc foo1() {.testTmpl1.} = discard
