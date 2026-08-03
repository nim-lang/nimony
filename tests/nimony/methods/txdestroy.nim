import std/syncio
import deps/mxdestroy

type
  PanelObj = object of NodeObj
    lbl: Label
    labels: seq[Label]
  Panel = ref PanelObj

proc use(l: Label): int = childCount(l)

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

main()
