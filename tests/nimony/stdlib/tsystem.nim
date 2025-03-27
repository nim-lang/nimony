import std/[syncio]

proc testnotin(s: string): bool =
  for c in items s:
    if c notin {'A'..'Z', 'a'..'z'}:
      return false

  return true

assert testnotin "abc"
assert testnotin "AZaz"
assert not testnotin "1"
assert not testnotin "abc "
