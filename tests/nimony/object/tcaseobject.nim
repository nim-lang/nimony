type Foo = object
  case x: bool
  of false:
    y: int
  of true:
    z: string
    t: bool
