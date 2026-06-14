import deps / mcustompragmas

proc withArg() {.importedPragma: true.} =
  discard

proc withoutArg() {.importedMarker.} =
  discard

withArg()
withoutArg()
