# `strutils.formatFloat`. The C library's `%f`/`%e`/`%g` is reproduced from the
# exact decimal expansion of the double, so this path has no `snprintf` to call
# — which is what makes it usable here at all: wasm has no variadic calling
# convention, so a `{.varargs.}` import cannot be typed and the MODULE is
# rejected before it runs. Both legs must agree on every digit, ties included.
import std/[syncio, strutils]

const values: array[14, float] = [
  0.0, -0.0, 1.0, 0.5, 2.5, 9.995, 0.1, 123.456, 1234.567,
  0.00000000001, 1e20, 1e-7, 3.141592653589793, 1.7976931348623157e308
]

for v in values:
  for p in [-1, 0, 1, 2, 6, 16, 20]:
    echo formatFloat(v, ffDefault, p), "|", formatFloat(v, ffDecimal, p), "|",
         formatFloat(v, ffScientific, p)
echo formatFloat(Inf), " ", formatFloat(-Inf, ffDecimal, 3), " ", formatFloat(NaN, ffScientific, 3)
