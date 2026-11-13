# The prover keeps a global's facts nowhere a `func` is the only code that runs
# (doc/language.md, "Aliasing"), so sem must reject a `func`, and a
# `.noSideEffect` closure, that writes a global.
{.feature: "assumeSync".}
var g = 3
func writesGlobal() =
  g = 5
proc main() =
  let cl = proc () {.closure, noSideEffect.} =
    g = 5
  cl()
writesGlobal()
main()
