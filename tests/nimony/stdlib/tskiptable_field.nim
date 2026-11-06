# `initSkipTable(result.table, sub)` hung in `find` (nimony#2906, item 3): the
# zero-init of `array[char, int]` that `M()` runs looped on a `char` counter
# up to `high(char)`, which wraps. Covers fields of `result`, a `ref` result,
# a nested field and a `var` parameter, plus `default` of other full-range
# index types.

import std/syncio
import std/strutils

type
  M = object
    needle: string
    table: SkipTable
  R = ref object
    table: SkipTable
  W = object
    inner: M

func initM(sub: string): M =
  result = M()
  initSkipTable(result.table, sub)
  result.needle = sub

proc initNoCtor(sub: string): M =
  result = default(M)
  initSkipTable(result.table, sub)
  result.needle = sub

proc initRef(sub: string): R =
  result = R()
  initSkipTable(result.table, sub)

proc initNested(sub: string): W =
  result = W()
  initSkipTable(result.inner.table, sub)
  result.inner.needle = sub

proc viaVar(m: var M; sub: string) =
  initSkipTable(m.table, sub)
  m.needle = sub

let m = initM("abc")
echo find(m.table, "xxabc", m.needle)
let m2 = initNoCtor("abc")
echo find(m2.table, "xxabc", m2.needle)
let r = initRef("abc")
echo find(r.table, "xxabc", "abc")
let w = initNested("abc")
echo find(w.inner.table, "xxabc", w.inner.needle)
var m3 = M()
viaVar(m3, "abc")
echo find(m3.table, "xxabc", m3.needle)

type E = enum e0, e1, e2
var b: array[E, int] = default(array[E, int])
var c: array[uint8, int] = default(array[uint8, int])
var d: array[bool, int] = default(array[bool, int])
echo b[e2], c[255], d[true]
