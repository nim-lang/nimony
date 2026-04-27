# Regression test: a type with a `.error.` =copy hook embedded as a field
# should not produce spurious "=copy is not available" diagnostics from
# synthesized hooks the code never actually calls. Before the fix in
# hexer/duplifier the hook-check walked into the body of a lifted
# =copy/=wasMoved and flagged the inner call, even though that outer hook
# was itself marked ErrorP and thus unreachable.

import std/assertions

type
  Inner = object
    data: ptr int
    len: int

proc `=destroy`(x: Inner) =
  if x.data != nil: discard
proc `=copy`(dest: var Inner; src: Inner) {.error.}
proc `=wasMoved`(dest: var Inner) =
  dest.data = nil
  dest.len = 0

type
  Outer = object
    a: int
    b: Inner

proc sink1(o: sink Outer): int =
  result = o.a

proc useInner =
  var o = Outer(a: 42, b: Inner(data: nil, len: 0))
  assert sink1(o) == 42

useInner()
