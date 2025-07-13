import std/syncio

template testTmpl1(procdef: untyped) {.untyped.} =
  let s = astToStr(procdef)
  echo "`" & s & "`"

template testTmplDup(procdef: untyped) {.untyped.} =
  procdef
  procdef

proc foo1() {.testTmpl1.} = discard
proc foo2() {.testTmplDup, testTmpl1.} = discard
proc foo3() {.testTmplDup, testTmpl1, testTmplDup.} = discard
