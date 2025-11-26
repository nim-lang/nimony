import std/syncio

template testTmpl0(prefix, suffix: string; procdef: untyped) {.untyped.} =
  let s = astToStr(procdef)
  echo prefix & s & suffix

template testTmpl0(circumfix: string; procdef: untyped) {.untyped.} =
  testTmpl0(circumfix, circumfix, procdef)

template testTmpl1(procdef: untyped) {.untyped.} =
  testTmpl0("`", "`", procdef)

template testTmplDup(procdef: untyped) {.untyped.} =
  procdef
  procdef

proc foo1() {.testTmpl1.} = discard
proc foo2() {.testTmplDup, testTmpl1.} = discard
proc foo3() {.testTmplDup, testTmpl1, testTmplDup.} = discard
proc foo4() {.testTmpl0("<", ">").} = discard 1 + 1
proc foo5() {.testTmpl0("'"), testTmpl0("{", "}").} = discard 2 + 2
proc foo6() {.testTmpl0("(_", "_)"), testTmplDup, testTmplDup.} = discard
proc foo7() {.testTmplDup, testTmplDup, testTmplDup, testTmpl0("#[", "]#").} = discard
