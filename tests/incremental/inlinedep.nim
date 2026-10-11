# Imported by `sample.nim` for the `inline-dep` phase of hastur's
# incremental-build regression (`hastur incremental`). Its body is edited and
# restored in place by `src/hastur/incrementaltests.nim`, so keep it tiny and
# keep `bump` inline: the point of the phase is that the callee's body is
# SPLICED into the importer's generated code, which is what makes the
# importer's codegen depend on this file (nim-lang/nimony#1897).

proc bump*(x: int): int {.inline.} =
  x + 1

proc scaled*(x: int): int =
  ## NOT inline, and big enough that no inliner takes it: the `body-dep` phase
  ## edits the factor and expects only this module's code to be regenerated.
  var acc = 0
  var i = 0
  while i < x:
    acc = acc + x * 3
    inc i
  result = acc div x
