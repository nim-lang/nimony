# A default value is substituted at the call site, so it must be a constant
# expression just like an explicit argument.

var v = 1
proc f(x: static[int] = v): int = x
