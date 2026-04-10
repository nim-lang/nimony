func main =
  var x: (nil ptr int) = nil
  if x != nil:
    discard x[]
    x[] = 123

func bar(x: int) =
  var m = x

func foo(x: var int) =
  let s = x
  bar(s)
  bar(x)

var m = 12
foo(m)
