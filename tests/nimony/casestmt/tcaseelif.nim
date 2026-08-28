import std / syncio

# Nim's `case` admits an `elif` chain after the `of` branches; nimony's `sem`
# lowers `of`* `else`? and nothing else. It used to drop the rest in silence:
# the `of` loop stopped at the `elif`, the `else` test saw an `elif` and
# declined, and the emitted `case` was missing every branch from there on. A
# `case`+`elif` in `hexer/coro_transform` then compiled clean and produced a
# hexer whose `coroTr` had lost its default branch, which broke the self-host.
# Until the form is lowered, it has to be an error rather than a deletion.

proc classify(s: string; flag: bool): string =
  case s
  of "jmp":
    result = "JMP"
  of "lab":
    result = "LAB"
  elif flag:
    result = "FLAG"
  else:
    result = "OTHER"

echo classify("jmp", false)
