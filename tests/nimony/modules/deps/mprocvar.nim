proc noop(size: int) {.nimcall.} = discard

# A not-nil proc type has no default value, so the variable states one: what
# the importing tests exercise is the cross-module lookup of a proc VARIABLE.
var procvar*: proc (size: int) {.nimcall.} = noop
