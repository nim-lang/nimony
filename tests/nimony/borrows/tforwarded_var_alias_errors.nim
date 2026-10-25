# A `var` parameter forwarded to another `var` parameter carries no `haddr`,
# but must be checked for aliasing like a local is (issue #2766).

type
  Item = object
    name: string
    parts: seq[string]
  Owner = object
    items: seq[Item]

proc use(o: var Owner; it: Item) =
  o.items.add Item(name: "filler")

proc swapBoth(a, b: var Owner) =
  discard

proc run(o: var Owner) =
  use(o, o.items[0])

proc runMut(o: var Owner) =
  swapBoth(o, o)

proc runOk(o: var Owner; other: Owner) =
  use(o, other.items[0])
  let first = o.items[0]
  use(o, first)

var o = Owner(items: @[Item(name: "first", parts: @["a", "b"])])
run(o)
runMut(o)
var other = o
runOk(o, other)
