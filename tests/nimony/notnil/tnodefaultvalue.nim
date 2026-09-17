# A not-nil ref has no default value, and neither has anything built out of
# one: zeroed storage spells `nil`, which is the single value the type rules
# out. A global is where this bites -- it is not covered by the initialization
# analysis (it is readable from another module and another thread), so its
# storage is simply zeroed.
type Foo = ref object
  x: int = 42

var a: array[8, Foo]
let b = a[0]

var scalar: Foo

type Pair = object
  f: Foo
  n: int
var aggregate: Pair

# A field that states a default of its own answers for itself:
type WithDefault = object
  f: Foo = Foo(x: 1)
  n: int
var fine: WithDefault
