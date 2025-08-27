import std/assertions

template typedTempl() =
  proc injectedProc() {.inject.} = discard
  proc gensymProc() {.gensym.} = discard
  let injectedLocal {.inject.} = 123
  let gensymLocal {.gensym.} = 456
  type InjectedType {.inject.} = object
    field: int
  type GensymType {.gensym.} = object
    field: int
  type Enum {.inject.} = enum e1, e2

typedTempl()

injectedProc()
assert injectedLocal == 123
let obj = InjectedType(field: 456)
let efld: Enum = e1

assert not declared(gensymProc)
assert not declared(gensymLocal)
assert not declared(GensymType)
