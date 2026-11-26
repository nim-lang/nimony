# A copy of a derived object (`=dup`) copies the inherited fields and keeps
# the object's type header: with a parent whose fields need no hooks, with a
# parent that has a hooked field, and with case-object parents, one with a
# hooked field in a branch. A
# user-written `=dup` of the base type copies the base part of a derived
# object, as it copies a value of the base type; so does a `=copy` written
# without a `=dup`. A class whose only copy hook is a written `=copy` keeps its
# type header too.
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

  HookedCase = object of RootObj
    case hk: K
    of ka: hs: string
    of kb: hn: int
  HookedCaseChild = object of HookedCase
    name: string

  HookBase = object of RootObj
    x: int
  HookDerived = object of HookBase
    name: string
  HookLeaf = object of HookDerived
    tag: string

  CopyBase = object of RootObj
    x: int
  CopyDerived = object of CopyBase
    name: string

  OwnCopy = object of Base
    name: string

proc `=dup`(b: HookBase): HookBase = HookBase(x: b.x + 1000)
proc `=copy`(dest: var CopyBase; src: CopyBase) = dest.x = src.x + 2000
proc `=copy`(dest: var OwnCopy; src: OwnCopy) =
  dest.x = src.x + 5
  dest.name = src.name

proc isDerived(b: Base): bool = b of Derived
proc isTagged(b: TagBase): bool = b of Tagged
proc isChild(b: CaseBase): bool = b of CaseChild
proc isHookedCaseChild(b: HookedCase): bool = b of HookedCaseChild
proc isHookDerived(b: HookBase): bool = b of HookDerived
proc isHookLeaf(b: HookBase): bool = b of HookLeaf
proc isCopyBase(b: RootObj): bool = b of CopyBase
proc isCopyDerived(b: CopyBase): bool = b of CopyDerived
proc isOwnCopy(b: Base): bool = b of OwnCopy

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

  let hc = HookedCaseChild(hk: ka, hs: "a branch string held on the heap, long enough", name: "a name long enough to be held on the heap")
  var hd = hc
  echo hd.hk, " ", hd.hs.len, " ", hd.name.len, " ", isHookedCaseChild(hd), " ", hc.hs.len

  let h = HookDerived(x: 42, name: "a name long enough to be held on the heap")
  var g = h
  var a = HookDerived(x: 1, name: "a")
  a = h
  echo g.x, " ", a.x, " ", isHookDerived(g), " ", isHookDerived(a), " ", h.name.len

  let l = HookLeaf(x: 7, name: "a name long enough to be held on the heap", tag: "a tag held on the heap, long enough")
  var m = l
  echo m.x, " ", isHookLeaf(m), " ", m.tag.len, " ", l.tag.len

  let cb = CopyBase(x: 7)
  var cc = cb
  let cd = CopyDerived(x: 42, name: "a name long enough to be held on the heap")
  var ce = cd
  echo cc.x, " ", isCopyBase(cc), " ", ce.x, " ", isCopyDerived(ce), " ", cb.x, " ", cd.name.len

  let o = OwnCopy(x: 1, name: "a name long enough to be held on the heap")
  var q = o
  echo q.x, " ", isOwnCopy(q), " ", q.name.len, " ", o.name.len

main()
