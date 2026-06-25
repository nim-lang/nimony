## Local bindings in an inner scope must shadow module names for dot lhs lookup.
import std/os
import std/assertions

proc check =
  type OsWrapper = object
    v: int
  let os = OsWrapper(v: 99)
  assert os.v == 99

check()
