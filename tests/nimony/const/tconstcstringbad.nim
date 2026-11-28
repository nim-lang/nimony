# Only string literals and `const` strings convert to `cstring`.
proc takesC(s: cstring) = discard

let s = "runtime"
takesC s
