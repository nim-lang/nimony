# Regression: an if-expression argument to a passive call. The xelim temp
# for the if-result (`x.N, string) is minted by the OUTER pipeline; the
# nested per-coroutine njvl run (treIteratorBody) then hoists `len(leg)`
# into its own xelim temp — which must NOT restart the counter, or both
# temps share one SymId and the coroutine frame gets a single slot used
# as both i64 and string (C: nimStrWasMoved on a _Bool; in bigger shapes
# `NI64 + struct string` in newStringOfCap).
import std / syncio

proc sendIt(s: string) {.passive.} =
  echo s

proc streamLegs(leg: string) {.passive.} =
  sendIt((if leg.len > 0: leg else: "empty"))

streamLegs("a")
streamLegs("")
