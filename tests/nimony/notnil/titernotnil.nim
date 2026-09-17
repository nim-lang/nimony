# An iterator TYPE -- the type of a first-class closure-iterator VALUE -- is a
# pointer pair like any other closure, so it is not-nil by default exactly as a
# proc type is: the same slot-0 marker (doc/tags.md), the same `nil` prefix to
# opt out.
type It = iterator (): int

var nilable: nil It = nil   # fine: states that it can be nil
var assigned: It = nil      # Error: `nil` into a not-nil iterator value
var uninit: It              # Error: no default value to zero-initialize with

proc take(x: It) = discard
take(nil)                   # Error: same, as an argument
