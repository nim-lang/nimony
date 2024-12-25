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

when false: # enable when `isImportedMain2` works
  when isImportedMain2:
    discard "bad"
  else:
    discard "good"

proc `not`*(x: bool): bool {.magic: Not.}

when not true:
  discard "bad"
else:
  discard "good"

when false: # enable when `isImportedMain2` works
  when not isImportedMain2:
    discard "good"
  else:
    discard "bad"

when false:
  proc defined*(x: untyped): bool {.magic: Defined.}

  when defined(nimony):
    discard "good"
  else:
    discard "bad"

  when defined(undefinedDefine):
    discard "bad"
  else:
    discard "good"

  when defined(nimony) and not defined(undefinedDefine):
    discard "good"
  else:
    discard "bad"

  when defined(undefinedDefine) or not defined(nimony):
    discard "bad"
  else:
    discard "good"
