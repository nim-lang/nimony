# A true redeclaration in an untyped template body is an error, not a crash.
template body() {.untyped.} =
  var size = 1
  var size = 2

proc f() =
  body()

f()
