proc passiveProc() {.passive.} = discard
proc regularProc() = discard
let x: proc() {.passive.} = regularProc