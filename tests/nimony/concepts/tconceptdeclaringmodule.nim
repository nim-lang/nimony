## A concept requirement is looked up in the module that *declares* the
## concept, not just where the check happens: `std/hashes` is not imported
## here, so `hash` is in scope neither locally nor on the checked types
## (`string` and `seq` belong to `system`). `Table`/`HashSet` reach the same
## rule through inheritance: `tables.Keyable` inherits its `hash` requirement
## from `hashes.Hashable`, so it resolves in `std/hashes`, not in `std/tables`.

import std/[tables, sets, assertions]
import deps/mconcepthashable

assert sameHash("abc", "abc")
assert not sameHash(1, 2)
assert sameHash(@['a'], @['a'])

var t = initTable[string, int]()
t["abc"] = 1
assert t.getOrDefault("abc") == 1

var u = initTable[(string, char), int]()
u[("a", 'b')] = 7
assert u.getOrDefault(("a", 'b')) == 7

var s = initHashSet[seq[int]]()
s.incl @[1, 2]
assert @[1, 2] in s
