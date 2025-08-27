import deps/mwhen

when true:
  discard "good"
else:
  discard "bad"

when false:
  discard "bad"
else:
  discard "good"

when isMainModule:
  discard "good"
else:
  discard "bad"

when isImportedMain1:
  discard "bad"
else:
  discard "good"

when isImportedMain2:
  discard "bad"
else:
  discard "good"

proc `not`*(x: bool): bool {.magic: Not.}

when not true:
  discard "bad"
else:
  discard "good"

when not isImportedMain2:
  discard "good"
else:
  discard "bad"
