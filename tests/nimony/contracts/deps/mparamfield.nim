# issue #2944: an accessor-shaped routine read across a module boundary.
type Pair* = object
  a*, b*: int
proc total*(b: Pair): int = b.a + b.b
