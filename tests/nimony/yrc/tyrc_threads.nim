# `--mm:yrc` with refs shared between threads: every worker walks a shared
# cyclic graph (atomic increments, deferred decrements from many threads),
# grows it under a lock (`seq.add` fences against captures on other threads),
# and builds cycles of its own, some pointing into the shared graph. Workers
# collect concurrently with each other and with the mutators; the ones that
# exit hand their queued decrements, candidates and heaps over. In the end
# every node must have been destroyed exactly once.
{.feature: "lenientnils".}
import std/[syncio, rawthreads, locks, assertions]

type
  NodeObj = object
    id: int
    kids: seq[Node]
    parent: Node
    name: string
  Node = ref NodeObj

var created, destroyed: int

proc `=destroy`(n: NodeObj) =
  discard atomicAddFetch(addr destroyed, 1, ATOMIC_RELAXED)
  `=destroy`(n.kids)
  `=destroy`(n.parent)
  `=destroy`(n.name)

proc newNode(id: int; parent: Node): Node =
  discard atomicAddFetch(addr created, 1, ATOMIC_RELAXED)
  result = Node(id: id, parent: parent, name: "n" & $id)

var shared: Node
var sharedLock: Lock

proc buildShared() =
  shared = newNode(0, nil)
  for i in 1..50:
    let k = newNode(i, shared)
    for j in 0..<5:
      k.kids.add newNode(i*100+j, k)
    shared.kids.add k
  shared.parent = shared.kids[0]  # a cycle through the root

proc walk(n: Node; depth: int): int =
  result = n.id
  if depth > 0:
    for k in n.kids:
      let tmp = k          # dup: atomic inc; scope end: deferred dec
      result = result + walk(tmp, depth - 1)

proc worker(arg: pointer) {.nimcall.} =
  let tid = cast[int](arg)
  var sum = 0
  for round in 0..<200:
    # garbage cycles, some pointing into the shared graph
    var a = newNode(tid * 1_000_000 + round, nil)
    var b = newNode(tid * 1_000_000 + round + 500_000, a)
    a.kids.add b
    a.parent = b
    if round mod 7 == 0:
      acquire sharedLock
      let s = shared.kids[round mod shared.kids.len]
      release sharedLock
      b.kids.add s
    # read the shared graph concurrently with other threads' collections
    acquire sharedLock
    let root = shared
    sum = sum + walk(root, 2)
    release sharedLock
    # grow the shared graph under the lock: `add` fences against captures
    if round mod 10 == 0:
      acquire sharedLock
      let k = newNode(-round, shared)
      shared.kids.add k
      release sharedLock
  discard sum

const NumThreads = 8

proc main =
  initLock sharedLock
  buildShared()
  var threads {.noinit.}: array[NumThreads, RawThread]
  for i in 0..<NumThreads:
    try:
      create(threads[i], worker, cast[pointer](i + 1))
    except:
      quit "cannot create thread"
  for i in 0..<NumThreads:
    join threads[i]
  shared = nil
  GC_fullCollect()
  let c = atomicLoadN(addr created, ATOMIC_RELAXED)
  let d = atomicLoadN(addr destroyed, ATOMIC_RELAXED)
  echo "created == destroyed: ", c == d, " (", c, " ", d, ")"

main()

{.feature: "assumeSync".}  # test program: globals shared freely
