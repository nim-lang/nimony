import std/syncio

# Tests if Nimony detects inner procs that uses local variables declared in enclosing proc.
# Remove this test when closures are supported.

proc outer =
  var x = 1
  proc inner =
    echo x
  inner()

outer()
