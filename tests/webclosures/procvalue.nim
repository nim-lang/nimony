## A proc VALUE converted to a closure has no env-taking bridge on wasm:
## refused at compile time instead of trapping at the call.
import closlib

proc named(n: int): int = n + 1

proc main =
  var p: proc (n: int): int {.nimcall.} = named
  let h = Holder(f: p)
  discard callHere(h, 1)

main()
