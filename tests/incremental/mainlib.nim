## Library half of the `isMainModule` phase of the incremental suite: built
## as a program of its own first, then imported by `mainuser.nim`.
import std/syncio

proc libValue*(): int = 42

when isMainModule:
  echo "mainlib-as-main"
