# issue #2244
# A closure proctype nested inside a named type declaration must be lifted to
# the same `(fn, env)` tuple shape as every other position, otherwise the type
# side keeps a bare function pointer while the values are already tuples.
import std/syncio

type
  ReactorProc* = proc() {.closure.}
  Pool* = object
    reactors: seq[ReactorProc]

var p = Pool(reactors: @[])

proc main() =
  var x = 42
  p.reactors.add(proc() {.closure.} =
    echo "x=", x
  )
  var y = 7
  p.reactors.add(proc() {.closure.} =
    echo "y=", y
  )

main()
echo p.reactors.len
