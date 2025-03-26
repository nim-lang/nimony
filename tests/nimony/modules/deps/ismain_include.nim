
proc isMainInclude*(): bool =
  when isMainModule:
    result = true
  else:
    result = false
