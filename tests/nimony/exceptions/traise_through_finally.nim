# A raising call inside a `try`/`finally` (no `except`) — the same shape `defer`
# lowers to — must STILL be rejected in a proc not declared `.raises`: a
# `finally` does not catch, so the exception escapes the proc. Regression test
# for a frontend bug (derefs.nim) where a bare try/finally wrongly granted
# "can raise" permission, letting an unhandled raise slip past sem and miscompile
# downstream into a malformed error-propagation tuple.
import std / tables

proc bad(): string =
  var t = initTable[string, string]()
  t["k"] = "v"
  try:
    result = t["k"]   # `Table.[]` raises `KeyError`; `finally` won't catch it
  finally:
    discard

discard bad()
