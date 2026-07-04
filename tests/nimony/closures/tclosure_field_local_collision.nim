# per-module SymId collision: a foreign type's field `writes` and a local
# `var writes` (renamed by for-loop iterator inlining) must not clash
import deps/mpass
import std/syncio

var passes: seq[Pass] = @[]
passes.add Pass(writes: @[1, 2, 3])
passes.add Pass(writes: @[])

for p in passes:
  var writes = false
  for w in p.writes:
    echo w
    writes = true
  echo writes
