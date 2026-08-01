# issue #1293
# should prints both the error in the procedure name and errors in arguments

undeclaredProc(
  undeclaredVar1, undeclaredVar2)

undeclaredTmpl:
  proc foo() {.wrongPragma1: "", wrongPragma2: "".} = discard
