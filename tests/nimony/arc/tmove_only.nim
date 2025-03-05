
import std / syncio

type
  MyObj = object
    a: int
    b: int

proc `=copy`(dest: var MyObj, src: MyObj) {.error.}
proc `=dup`(src: MyObj): MyObj {.error.}

proc `=destroy`(o: MyObj) = discard

proc use(o: MyObj) = discard

proc main(inp: string) =
  var o = MyObj(a: 1, b: 2)
  let other = o
  use o

main("b")
