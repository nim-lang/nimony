## A toplevel let/var whose explicit type is not yet available in the
## signature phase (here: an undeclared `T`) must NOT be resolved on demand
## for a `when` condition — the reference degrades to a clean "undeclared"
## instead of a wrong early resolution. nim-lang/nimony#1974.

var x: T
when x is int:
  discard
