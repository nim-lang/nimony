type CB = proc () {.closure.}

proc viaParam(cb: proc () {.closure.}) {.importjs: "#()".}
proc viaResult(): proc () {.closure.} {.importjs: "f()".}
proc viaAlias(cb: CB) {.importjs: "#()".}

# accepted: bare and nimcall procs
proc okBare(cb: proc ()) {.importjs: "#()".}
proc okNimcall(cb: proc () {.nimcall.}) {.importjs: "#()".}

# a JS template cannot report a Nim error
proc okRaises(): int {.importjs: "1".}
proc badRaises(): string {.importjs: "'x'", raises.}

# a pointer to one is an opaque handle, an object holding one an address
proc okPtr(cb: ptr CB) {.importjs: "#()".}
