# closure call through a for-loop variable over seq[proc]
import std/syncio

var procs: seq[proc () {.closure.}] = @[]
procs.add(proc () {.closure.} = echo "hi")
for p in procs:
  p()
