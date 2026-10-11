# A routine with a `.requires`, split by this module into a guarded copy and a
# guard-free one (`contracts_fir.splitsOnRequires`). See `treqsplit_import`.

proc half*(x: int): int {.requires: x > 0.} =
  result = x div 2
