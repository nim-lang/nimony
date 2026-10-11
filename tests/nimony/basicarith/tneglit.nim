# The inliner substitutes a negative literal into `abs`'s `-y`: lengc must
# not emit that negation as C's `--3.0`.
import std/[math, syncio]

proc main =
  echo floorMod(8.0, -3.0)
  echo euclMod(8.0, -3.0)
  echo abs(-3)

main()
