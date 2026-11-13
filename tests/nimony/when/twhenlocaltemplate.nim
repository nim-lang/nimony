## A signature-phase `when` that reaches a deferred toplevel `let`/`var`
## through a template argument resolves it on demand (nim-lang/nimony#1974)
## like a direct reference: the template's argument scope is rolled back,
## but the toplevel declaration stays.

import std / syncio

template isSeq(x: untyped): bool = x is seq[int]

let s: seq[int] = @[1, 2, 3]
when isSeq(s):
  echo "s-seq"
else:
  echo "s-other"

var n: int = 4
when isSeq(n):
  echo "n-seq"
else:
  echo "n-other"

# both locals stay usable in the body phase
echo s.len + n
