# Companion to ttemplate_frames.nim for nim-lang/nimony#1987: a variable
# *declared inside* a template expansion.
#
# `#dbg_declare` names both a DILocalVariable scope and a DILocation scope.
# Under an active inlined frame both must be the template's synthetic
# DISubprogram and the location must carry the same `inlinedAt` chain — LLVM's
# verifier rejects the module when they disagree. `template t = (var x = ...)`
# is ordinary code, so this is a correctness requirement, not polish.

template withTemp(a: int; b: int) =
  var t: int = a
  t = t + b
  a = t

proc run2(x: var int; y: int) {.exportc.} =
  withTemp(x, y)
