{.pragma: customPragma, noinit, discardable, tags: [].}

proc foo(): int {.customPragma.} = discard

foo()
