type
  bool* {.magic: Bool.} = enum ## Built-in boolean type.
    false = 0, true = 1

const isMainModule* {.magic: IsMainModule.} = false

when isMainModule:
  const isImportedMain1* = true
else:
  const isImportedMain1* = false

const isImportedMain2* = isMainModule
