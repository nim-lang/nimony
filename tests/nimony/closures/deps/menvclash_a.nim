## Half of the `tenv_name_clash` pair: a proc named `shared` that captures one
## local, so lambda lifting mints the environment type `shared.0.env.<module>`
## with a single field. `menvclash_b` mints the SAME name with a different
## layout — see the consumer.

import std / syncio

proc shared*() =
  var a = 11
  proc inner() {.closure.} = echo a
  inner()
