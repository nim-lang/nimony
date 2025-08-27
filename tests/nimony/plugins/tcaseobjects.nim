import std / syncio


template renderTree(s: typed) {.plugin: "deps/mrender".}

renderTree:
  type
    Foo = object
      case x: bool
      of true:
        a, b: int
        case y: bool
        of false:
          m: int
        else:
          s: float
          g: int
      of false:
        c: float