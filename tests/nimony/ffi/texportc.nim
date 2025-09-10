block:
  type
    X {.exportc.} = object
      v: int

  {.emit:"""
  X x = { 1234 };
  """.}

  proc foo() {.exportc.} =
    discard 123

  foo()