# Tests if Nimony detects code that requires closures.
# Remove this test when closures are supported.

# should not report closure error in following code
var x = 0
proc foo =
  discard x

proc bar =
  proc nested =
    discard x

# should report the error in following code.
proc baz =
  var x = 1
  proc nested =
    discard x
