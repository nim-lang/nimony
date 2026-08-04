## Regression test for the closure-in-seq + openArray-iteration C type
## mismatch (nim-lang/nimony#2281). `for i, r in seqOfClosures` lowers to
## the sem-inlined openarrays helpers (`toOpenArray`, `rawData`, `\5B\5D`).
## A forward `(call rawData)` inside `toOpenArray`'s body hoists into a
## cursor temp typed from the stale pre-lambdalifting symbol (bare closure
## `proctype`) while the lifted `rawData` returns the `(tuple <fn> (ref
## RootObj))` tuple — lengcgen then emits incompatible pointer types (a
## clang warning, a gcc hard error on Linux). The temp must be typed from
## the current-IR signature, i.e. the lifted tuple.

import std / syncio

var offset = 5
type Reactor = proc(timeoutMs: int): bool {.closure.}
var reactors: seq[Reactor]
reactors.add((proc(timeoutMs: int): bool = timeoutMs > offset))
reactors.add((proc(timeoutMs: int): bool = timeoutMs < offset))
reactors.add((proc(timeoutMs: int): bool = timeoutMs == offset))
for i, reactor in reactors:
  echo i, " ", reactor(i)
