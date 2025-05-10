const bufsize = 65  # Buffer size used in formatfloat in Nim 2

# TODO Replace c_snprintf with a better float to string algorithm.
# See formatfloat module in Nim 2.
proc c_snprintf(str: out array[bufsize, char]; n: uint; fmt: cstring): int32 {.header: "<stdio.h>",
                                    importc: "snprintf", varargs, noSideEffect.}

proc addFloat*(result: var string; x: float) {.inline.} =
  var buf {.noinit.}: array[bufsize, char]
  let n = c_snprintf(buf, bufsize.uint, "%g", x)
  let oldLen = result.len
  let newLen = oldLen + n
  result.setLen(newLen)
  for i in 0 .. (n - 1):
    result[oldLen + i] = buf[i]
