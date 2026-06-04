const bufsize = 65  # Buffer size used in formatfloat in Nim 2

func c_snprintf(str: out array[bufsize, char]; n: uint; fmt: cstring): int32 {.header: "<stdio.h>",
                                    importc: "snprintf", varargs, noSideEffect.}
func c_strtod(str: cstring; endptr: ptr cstring): float64 {.header: "<stdlib.h>",
                                    importc: "strtod", noSideEffect.}

# Shortest `%g` precision (1..17) whose output round-trips back to `x`. 17 sig
# figs always round-trips a binary64, so the loop terminates. We pick the
# shortest so the NIF stays compact and float constants survive a write/read
# cycle exactly — `%g` at its default 6 figs silently truncated them.
const gfmts: array[1..17, cstring] = [
  "%.1g", "%.2g", "%.3g", "%.4g", "%.5g", "%.6g", "%.7g", "%.8g", "%.9g",
  "%.10g", "%.11g", "%.12g", "%.13g", "%.14g", "%.15g", "%.16g", "%.17g"]

func addFloat*(result: var string; x: float) =
  var buf {.noinit.}: array[bufsize, char]
  var n = 0
  for prec in 1 .. 17:
    # prec 17 always round-trips a binary64, so this terminates with `buf`
    # holding the chosen rendering. Non-finite `x` (nan/inf, reachable via `$`)
    # never compares equal and simply falls through at prec 17.
    n = c_snprintf(buf, bufsize.uint, gfmts[prec], x).int
    var endp {.noinit.}: cstring
    if c_strtod(cast[cstring](addr buf[0]), addr endp) == x:
      break
  # A NIF FloatLit must carry a '.'/'e'/'E' or it re-tokenizes as an IntLit;
  # `%g` prints whole-valued finite floats as bare integers ("0", "1", "100").
  # Append ".0" exactly when the result is a pure integer (digits and sign only),
  # which leaves "inf"/"nan"/exponential forms untouched.
  var pureInt = n > 0
  for i in 0 ..< n:
    if buf[i] notin {'0'..'9', '-', '+'}:
      pureInt = false
      break
  let oldLen = result.len
  result.setLen(oldLen + n + (if pureInt: 2 else: 0))
  for i in 0 ..< n:
    result[oldLen + i] = buf[i]
  if pureInt:
    result[oldLen + n] = '.'
    result[oldLen + n + 1] = '0'

func `$`*(x: float): string {.inline.} =
  result = ""
  result.addFloat x
