# Imported by `sample.nim` for the `live-edit` step of hastur's
# incremental-build regression (`hastur incremental`).

proc unusedProc*(x: int): int =
  ## Dead until the step has `sample.nim` call it: the edit that moves this
  ## module's live set and nobody else's. Recursive, so that no inliner folds
  ## it into the caller: it has to become a live symbol here.
  if x <= 0: result = 0
  else: result = 2 + unusedProc(x - 1)
