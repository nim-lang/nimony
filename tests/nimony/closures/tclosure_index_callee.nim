# closure call through seq indexing
import std/syncio

var procs: seq[proc () {.closure.}] = @[]
procs.add(proc () {.closure.} = echo "hi")
procs[0]()
