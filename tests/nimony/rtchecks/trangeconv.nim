
import std / [syncio]

# Explicit conversions to an ordinal `range` type are range-checked at runtime
# (the `RangeCheck` mode, on by default). In-range values pass through
# unchanged; the first out-of-range value aborts with a range error.

proc toDigit(x: int): range[0..9] =
  result = range[0..9](x)

proc main =
  for i in 0 .. 10:
    echo toDigit(i)
    flushFile(stdout)

main()
