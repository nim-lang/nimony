# `not` of an operand the optimizer folds to a literal is computed at the
# operation's width, not the literal's (#54).
import std/syncio
proc i32(p: int32) {.noinline.} =
  let x: int32 = not (p - p)
  echo x
proc i8(p: int8) {.noinline.} =
  let x: int8 = not (p - p)
  echo x
i32(5'i32)
i8(5'i8)
