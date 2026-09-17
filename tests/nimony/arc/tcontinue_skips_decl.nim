## A `continue` that skips the declaration of a local with a destructor. The
## Final IR spells it as a `jmp` to a label in front of the loop's back-edge;
## that label has to sit outside the scope the local is declared in, or the
## scope's end destroys a value that was never initialized on the `continue`
## path. Here the local is the temp holding `parts(name)` for the inner loop —
## the shape `parsegen`'s `analyse` had. Checked by valgrind.

import std/syncio

proc parts(s: string): seq[string] =
  result = @[s & "1", s & "2"]

proc main =
  var total = 0
  for name in @["a", "skip", "c", "skip", "e"]:
    if name == "skip":
      continue
    for p in parts(name):
      total += p.len
  echo total

main()
