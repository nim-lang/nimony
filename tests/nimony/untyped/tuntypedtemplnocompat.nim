import std / [syncio]

type
  Inner = object
    s: string
  Function = object
    inner: Inner

template emit(s2: string) {.untyped.} = f().inner.s.add s2

proc useGlobal =
  template f(): untyped {.untyped.} = myObj
  var myObj = Function(inner: Inner(s: "abc"))
  emit " xyz"

  echo myObj.inner.s

useGlobal()
