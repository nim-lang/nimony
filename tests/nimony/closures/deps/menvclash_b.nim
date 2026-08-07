## The other half of the `tenv_name_clash` pair. Same proc name as
## `menvclash_a.shared`, hence the same environment-type name — but two captures
## under different names, so a different layout AND different field names.

import std / syncio

proc shared*() =
  var p = 3
  var q = 5
  proc inner() {.closure.} = echo p * q + 2
  inner()
