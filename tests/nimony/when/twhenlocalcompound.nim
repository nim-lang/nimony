## A signature-phase `when` may inspect a toplevel `let`/`var` whose explicit
## type is a *compound* expression (e.g. `seq[int]`), not just a bare atom —
## the type is resolved on demand. Previously the atom-only gate excluded
## these. nim-lang/nimony#1974.

import std / syncio

let s: seq[int] = @[1, 2, 3]
when s is seq[int]:
  echo "s-seq"
else:
  echo "s-other"

# the local stays usable in the body phase
echo s.len
