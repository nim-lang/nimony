type Bar {.importc: "Bar", header: "bar.h".} = object
type Bar1 {.importc: "Bar1", header: "${path}/bar1.h".} = object

const header = "bar.h"
type Bar2 {.importc: "Bar", header: header.} = object
