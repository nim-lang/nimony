import std/syncio

# Regression: a branch whose whole body is a compile-time-false `when`
# sems to an empty statement list; `implicitlyDiscardable` (via `isNoReturn`)
# used to skip past its ParRi and overrun the cursor span (nifcursors
# `load` assertion). Both the plain and the value-yielding case shapes.

type Kind = enum kA, kB, kC

proc handle(k: Kind) =
  case k
  of kA:
    when defined(neverDefinedPlatform):
      echo "unreachable"
  of kB:
    echo "b"
  of kC:
    discard

handle(kA)
handle(kB)

proc describe(k: Kind): string =
  result = "none"
  case k
  of kA:
    when defined(neverDefinedPlatform):
      result = "a"
  of kB:
    result = "b"
  of kC:
    discard

echo describe(kA)
echo describe(kB)

# if-branch flavour of the same hole
proc viaIf(x: int) =
  if x == 1:
    when defined(neverDefinedPlatform):
      echo "unreachable"
  else:
    echo "else"

viaIf(1)
viaIf(2)
