# A block label that is already a symbol when the block is checked (a
# template's argument or body) is still found by `break` (#2640).
import std/syncio

echo block:
  proc run(): bool =
    block failed:
      if true: break failed
      return true
    return false
  run()

template t(): int =
  var r = 1
  block lbl:
    if true: break lbl
    r = 2
  r

proc p(): int =
  echo t()
  result = t()

echo t()
echo p()
