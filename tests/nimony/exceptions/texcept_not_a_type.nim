# `except ValueError:` is the Nim 2 form; here `ValueError` is an `ErrorCode`
# value. Each arm is reported, and a `defer` in the routine does not turn the
# error into a crash (#2807).

proc num(w: string): int {.raises.} =
  if w.len == 0: raise ValueError
  w.len

proc plain(w: string): int =
  defer: discard
  try:
    return num(w)
  except ValueError:
    discard
  0

proc notRaisable(w: string): int =
  defer: discard
  try:
    return num(w)
  except int:
    discard
  0

proc qualified(w: string): int =
  try:
    return num(w)
  except ErrorCode.ValueError as e:
    discard
  0

proc notRaisableAs(w: string): int =
  try:
    return num(w)
  except int as e:
    discard
  0

proc plainAs(w: string): int =
  try:
    return num(w)
  except ValueError as e:
    discard
  0

discard plain("x")
