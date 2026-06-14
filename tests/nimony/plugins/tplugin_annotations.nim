import plugins

proc emitAny(dest: var NifBuilder) {.ensuresNif: addedAny(dest).} =
  discard

proc emitNested(dest: var NifBuilder) {.ensuresNif: addedNested(dest).} =
  discard

var dest = createTree()
emitAny(dest)
emitNested(dest)
