## Regression test for the open-symbol / mixin behavior of generic
## bodies: when `wrap[K: Keyable](k: K): Hash = hash(k)` is instantiated
## from this module, the inner `hash(k)` call must consider the
## overloads visible at `mconcept_def_site`'s definition site (from
## `std/hashes`) — not just the overloads visible here, where the only
## `hash` in scope is the unrelated `hash[X,Y]` from
## `mconcept_other_hash`.
import deps / mconcept_def_site
import deps / mconcept_other_hash

let h = wrap[string]("foo")
discard h
