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

let a = mat(1, 2, 3, 4)
let b = mat(0, 1, 1, 0)
let c = mat(1, 1, 1, 1)
let d = mat(2, 0, 0, 2)
binaryCalls = 0
inplaceCalls = 0

chains(a, b, c, d)
var x = a
viaVar(x, b)

let top = Matrix(a: [[1.0, 0.0], [0.0, 1.0]]) + a + b  # global, object constructor
report "global", top
