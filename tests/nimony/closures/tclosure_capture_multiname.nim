import std/[syncio]

# Regression: a multi-name local declaration (`var a, b: T`) whose names are
# captured by a `.closure` proc crashed hexer with
#   [Bug] expected ')', but got: .
# sem splits `var startX, startY: float` into two uninitialized `var` decls
# (empty initializer = a DotToken value slot). lambdalifting.treLocal's captured
# branch only consumed the value token inside `if n.kind != DotToken`, so an
# empty initializer left the DotToken unconsumed and skipParRi hit it instead of
# the closing ParRi. Any captured local with no initializer trips this; the
# multi-name form is just the common way to produce one.

type Holder = ref object
  cb: proc(): float {.closure.}

proc makeVar(): Holder =
  var startX, startY: float   # multi-name var, both captured, empty init
  startX = 1.5
  startY = 2.5
  result = Holder(cb: proc(): float {.closure.} = startX + startY)

proc makeLet(a: float): Holder =
  let p, q: float = a         # multi-name let, both captured
  result = Holder(cb: proc(): float {.closure.} = p + q)

let hv = makeVar()
echo hv.cb()

let hl = makeLet(2.0)
echo hl.cb()
