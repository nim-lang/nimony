import std/syncio
import deps/mxdestroy

type
  PanelObj = object of NodeObj
    lbl: Label
    labels: seq[Label]
  Panel = ref PanelObj

proc use(l: Label): int = childCount(l)

# A subtype-typed local (`Label`) moved into a base-typed result (`Node`):
# `result = l` upcasts and moves, so the moved-from `l` needs `=wasMoved`
# through the upcast. The wasmoved arg must be `&l` (cast the address), not
# `&((Node*)l)` (address of a cast rvalue, which the C compiler rejects).
proc buildMovedIntoBase(): Node =
  let l: Label = newLabel()
  discard use(l)
  result = l

proc main() =
  var base = destroyCount()
  block:
    let l = newLabel()
    discard use(l)                 # scope-exit destroys l
  echo "scope ", destroyCount() - base

  base = destroyCount()
  let p = Panel()
  p.lbl = newLabel()
  discard use(p.lbl)
  p.lbl = newLabel()               # reassignment destroys the first
  discard use(p.lbl)
  echo "reassign ", destroyCount() - base

  base = destroyCount()
  p.labels = @[]
  p.labels.add(newLabel())
  p.labels.add(newLabel())
  discard use(p.labels[0]) + use(p.labels[1])
  p.labels = @[]                   # seq-clear destroys both
  echo "seqclear ", destroyCount() - base

  base = destroyCount()
  block:
    let n = buildMovedIntoBase()   # subtype moved into base -> wasmoved via upcast
    discard childCount(n)          # destroyed once here; double-free would print 2
  echo "movebase ", destroyCount() - base

main()
