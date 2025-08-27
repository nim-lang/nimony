import std / [syncio]

proc test(s: string) =
  for c in items s:
    if c notin {'A'..'Z', 'a'..'z'}:
      echo "yep"
    else:
      echo "no"

test "abc"
