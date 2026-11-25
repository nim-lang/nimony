# A copy of a derived object (`=dup`) copies the inherited fields and keeps
# the object's type header: with a parent whose fields need no hooks, with a
# parent that has a hooked field, and with a case-object parent.
import std/syncio

type
  Base = object of RootObj
    x: int
  Derived = object of Base
    name: string

  TagBase = object of RootObj
    tag: string
  Tagged = object of TagBase
    name: string

  K = enum ka, kb
  CaseBase = object of RootObj
    case k: K
    of ka: s: int
    of kb: n: int
  CaseChild = object of CaseBase
    name: string

proc isDerived(b: Base): bool = b of Derived
proc isTagged(b: TagBase): bool = b of Tagged
proc isChild(b: CaseBase): bool = b of CaseChild

proc main() =
  let d = Derived(x: 42, name: "a name long enough to be held on the heap")
  var e = d
  echo e.x, " ", isDerived(e), " ", d.name.len

  let t = Tagged(tag: "a tag long enough to be held on the heap", name: "another name held on the heap")
  var u = t
  echo u.tag.len, " ", u.name.len, " ", isTagged(u), " ", t.tag.len

  let c = CaseChild(k: kb, n: 7, name: "a name long enough to be held on the heap")
  var f = c
  echo f.k, " ", f.n, " ", isChild(f), " ", c.name.len

main()
