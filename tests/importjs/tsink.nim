# A `sink` parameter must not give an `importjs` proc a body: the template is
# still spliced at the call site.
proc take(s: sink string): int {.importjs: "#.length".}
proc log(x: int) {.importjs: "console.log(#)".}

log take("hello")
