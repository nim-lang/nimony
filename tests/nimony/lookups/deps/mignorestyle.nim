# Helper for `tignorestyle.nim`. Exports identifiers in a particular spelling
# so the importing test (which opts into `.feature: "ignoreStyle".`) can refer
# to them with varied casing / underscores.

proc myProcName*(x: int): int = x + 1

type
  HttpStatus* = enum
    httpOk, httpNotFound

proc useFromHere*(x: int): int = myProcName(x)
