import std/syncio
# Dispatching on a `sink` parameter: the receiver's type is `(sink (ref T))`.

type
  BaseObj = object of RootObj
  Base = ref BaseObj
  LeafObj = object of BaseObj
  Leaf = ref LeafObj

method kind(b: Base): int {.base.} = 0
method kind(l: Leaf): int = 1

proc plain(b: sink Base) =
  echo b.kind()

proc passiveJob(b: sink Base) {.passive.} =
  echo b.kind()

proc main() =
  plain(Leaf())
  passiveJob(Base())

main()
