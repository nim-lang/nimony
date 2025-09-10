type
  Foo = object
    when defined(abcd):
      x: int
    else:
      y: int
    z: int
    when defined(nimony):
      a: string
    else:
      b: bool
