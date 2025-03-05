
import std / syncio

type
  MyObj = object
    a: int
    b: int

proc `=copy`(dest: var MyObj, src: MyObj) {.error.}
proc `=dup`(src: MyObj): MyObj {.error.}

proc `=destroy`(o: MyObj) = discard

proc use[T](o: T) = discard

proc main(inp: string) =
  # see if a lifted copy is still prevented:
  var o = [MyObj(a: 1, b: 2), MyObj(a: 3, b: 4)]
  let other = o
  use o

main("b")
