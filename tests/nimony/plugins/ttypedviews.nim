import std / syncio

template describe(body: typed): string {.plugin: "deps/mtypedviews".}

const s = describe:
  let a = 1.5
  var b = 2
  const c = 3
  type Person = object
    name: string
    age: int
  proc f(x: float, y: int): float = x
  proc g[T](x: T): T = x
  for i in 0 ..< 3:
    discard

echo s
