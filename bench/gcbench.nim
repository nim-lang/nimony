## GCBench -- the Boehm/Ellis/Detlefs allocation benchmark, as ported to Nim in
## `tests/gc/gcbench.nim`. It builds and drops large binary trees, so its cost is
## almost entirely allocation plus reference-count traffic; the trees are built
## through `sink` parameters, which means almost no incRefs and a decRef on every
## node. That makes it the workload that shows what atomicArc's atomics cost
## relative to plain `--mm:arc`.
##
## Prints one line: the wall time of `main` in microseconds. Take a median of
## several pinned runs -- allocation benchmarks are noisy.

import std / [syncio, monotimes]

type
  PNode = ref TNode
  TNode = object
    left, right: PNode
    i, j: int

proc newNode(l, r: sink PNode): PNode =
  result = PNode(left: l, right: r)

const
  kStretchTreeDepth = 18    # about 16Mb
  kLongLivedTreeDepth = 16  # about 4Mb
  kArraySize = 500_000      # about 4Mb
  kMinTreeDepth = 4
  kMaxTreeDepth = 16

proc treeSize(i: int): int = (1 shl (i + 1)) - 1

proc numIters(i: int): int = 2 * treeSize(kStretchTreeDepth) div treeSize(i)

proc populate(iDepth: int; thisNode: PNode) =
  ## Build the tree top down, assigning to older objects.
  if iDepth <= 0: return
  new(thisNode.left)
  new(thisNode.right)
  populate(iDepth-1, thisNode.left)
  populate(iDepth-1, thisNode.right)

proc makeTree(iDepth: int): PNode =
  ## Build the tree bottom up.
  if iDepth <= 0:
    new(result)
  else:
    result = newNode(makeTree(iDepth-1), makeTree(iDepth-1))

proc timeConstruction(depth: int) =
  # Each tree dies at the end of its own iteration; Nimony's refs are non-nilable,
  # so the scope does what gcbench's `tempTree = nil` does in Nim.
  let iNumIters = numIters(depth)
  for i in 0 ..< iNumIters:
    var tempTree: PNode
    new(tempTree)
    populate(depth, tempTree)
  for i in 0 ..< iNumIters:
    let tempTree = makeTree(depth)
    if tempTree.i != 0: quit "unreachable"

proc main =
  # Stretch the memory space quickly.
  block:
    let tempTree = makeTree(kStretchTreeDepth)
    if tempTree.i != 0: quit "unreachable"

  # A long-lived tree, so not everything dies young.
  var longLivedTree: PNode
  new(longLivedTree)
  populate(kLongLivedTreeDepth, longLivedTree)

  # A long-lived array, filling half of it.
  var myarray = newSeq[float](kArraySize)
  for i in 0 ..< kArraySize div 2:
    myarray[i] = 1.0 / float(i + 1)

  var d = kMinTreeDepth
  while d <= kMaxTreeDepth:
    timeConstruction(d)
    d = d + 2

  # Keep both alive to here so the optimizer cannot drop the work.
  if longLivedTree == nil or myarray[1000] == 0.0:
    quit "gcbench failed"

let t0 = getMonoTime().ticks
main()
let dt = getMonoTime().ticks - t0
echo dt div 1000
