type Bar {.importc: "Bar", header: "bar.h".} = object
type Bar1 {.importc: "Bar1", header: "${path}/bar1.h".} = object
