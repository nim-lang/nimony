# A `set[char]` literal argument inside a `.passive` proc. The coroutine
# lifting stores the temporary in the frame with the set's lowered type,
# `(array (u 8) 32)`, whose literal length the default-value pass could not
# read ("cannot determine array length").
import std/[syncio, strutils]

proc trimmed(s: string): string {.passive.} =
  result = s.strip(chars = {'0', ' '}, trailing = false)

echo trimmed("  007")
