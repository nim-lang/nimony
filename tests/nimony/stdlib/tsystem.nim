import std/[assertions]

proc testnotin(s: string): bool =
  for c in items s:
    if c notin {'A'..'Z', 'a'..'z'}:
      return false

  return true

assert testnotin "abc"
assert testnotin "AZaz"
assert not testnotin "1"
assert not testnotin "abc "

# generic min/max:
assert min("a", "b") == "a"
assert min("b", "a") == "a"
assert max("a", "b") == "b"
assert max("b", "a") == "b"
