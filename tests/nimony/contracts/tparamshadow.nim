# The callee's parameter and the caller's local are both `i.0`: the
# precondition must substitute the argument `i + 1` for the parameter once,
# not again inside the argument.
import deps/mparamshadow

proc f(s: string): char =
  var i = 0
  if i + 1 < s.len:
    result = at(s, i + 1)
  else:
    result = ' '

discard f("ab")
