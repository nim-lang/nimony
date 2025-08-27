# See issue #1293

undeclaredProc(
  undeclaredVar1, undeclaredVar2)

undeclaredTmpl:
  proc foo() {.wrongPragma1: "", wrongPragma2: "".} = discard
