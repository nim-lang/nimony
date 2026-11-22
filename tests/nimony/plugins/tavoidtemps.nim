{.feature: "assumeSync".}
import std / syncio
import deps / mmatrix

proc report(label: string; m: Matrix) =
  echo label, " ", $m, " binary=", binaryCalls, " inplace=", inplaceCalls
  binaryCalls = 0
  inplaceCalls = 0

proc chains(a, b, c, d: Matrix) =
  let e = a*b + c - d          # the language guide's example: no temporaries
  report "chain", e

  let f = a - (b*c + d)        # the right operand gets one temporary
  report "nested", f

  var g = a * 2.0 + b          # scalar right operands are fine
  report "scalar", g

  let h = 2.0 * a + b          # starts at a float: left alone
  report "scalarleft", h

  let i = a + b                # a single operator saves nothing: left alone
  report "single", i

  let j = mat(1, 0, 0, 1) + a + b  # starts at a call of unknown type: left alone
  report "call", j

proc viaVar(x: var Matrix; y: Matrix) =
  let k = x + y + y            # starts at a `var` parameter
  report "varparam", k

proc assignments(a, b, c: Matrix) =
  var x = c
  x = a * b                    # `x = a; x *= b`: no temporary moved into `x`
  report "asgn", x

  x = a - (b + c + a)          # the right operand still gets one temporary
  report "asgnnested", x

  x = x + a                    # mentions `x`: left alone
  report "asgnself", x

  x = a + b * x                # mentions `x` further right: left alone
  report "asgnselfrhs", x

proc assignVar(x: var Matrix; y: Matrix) =
  x = y + y                    # a `var` parameter may alias `y`: left alone
  report "asgnvarparam", x

proc viaResult(a, b, c: Matrix): Matrix =
  result = a + b + c           # `result` is a local

let a = mat(1, 2, 3, 4)
let b = mat(0, 1, 1, 0)
let c = mat(1, 1, 1, 1)
let d = mat(2, 0, 0, 2)
binaryCalls = 0
inplaceCalls = 0

chains(a, b, c, d)
var x = a
viaVar(x, b)
assignments(a, b, c)
assignVar(x, b)
report "asgnresult", viaResult(a, b, c)

var gx = a
gx = b + c                     # a global: left alone
report "asgnglobal", gx

let top = Matrix(a: [[1.0, 0.0], [0.0, 1.0]]) + a + b  # global, object constructor
report "global", top
