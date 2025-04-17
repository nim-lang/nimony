type Foo = object
  case x: bool
  of false:
    y: int
  of true:
    z: string
    t: bool

type Bar = object
  case x: bool
  of false, true: discard

var foo = Foo()
foo = Foo(x: false, y: 123)
foo = Foo(x: true, z: "abc", t: false)
let a: bool = foo.x
let b: int = foo.y
let c: string = foo.z
let d: bool = foo.t

var bar = Bar()
bar = Bar(x: false)
bar = Bar(x: true)
let e: bool = bar.x
