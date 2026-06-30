# A bare `typedesc` parameter used to build a generic return type
# (`proc make(T: typedesc): Box[T]`) used to crash nimsem with an
# `addSubtree` "cursor at end?" assertion in `exprToType`. It must now
# report a normal error instead.
type Box[T] = object
  value: T

proc make(T: typedesc): Box[T] = discard
