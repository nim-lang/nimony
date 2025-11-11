type Bar {.importc: "Bar", header: "bar.h".} = object
type Bar1 {.importc: "Bar1", header: "${path}/bar1.h".} = object

const header = "bar.h"
type Bar2 {.importc: "Bar", header: header.} = object

import std/assertions

type
  Color* {.bycopy, importc, header: header.} = object
    r*: uint8
    g*: uint8
    b*: uint8
    a*: uint8

var color = Color(r: 8, g: 8, b: 8, a: 8)
assert color.r == 8
