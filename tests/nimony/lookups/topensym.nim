## An open symbol in a template body must be resolved again where the template
## is expanded, against the *expansion site's* scope — including its imports.
## Keeping only the declaration site's candidates made system's
## `template `in`(x, y) = contains(y, x)` unable to reach `strutils.contains`:
## `sub in str` failed while `contains(str, sub)` on the same line resolved.

import std / [syncio, strutils]
import deps / [mopensymtempl, mopensymop]

proc main =
  let s = "hello world"
  echo contains(s, "wor")
  echo "wor" in s
  echo "xyz" in s
  echo "xyz" notin s

  # `combined` is declared in a module that only knows the int and bool
  # overloads; the string one reaches it through this module's imports.
  # (Two decoys, not one: a name with a single declaration-site candidate is
  # still bound eagerly there and never becomes an `ochoice` at all.)
  echo combined(1, 2)
  echo combined("a", "b")

main()
