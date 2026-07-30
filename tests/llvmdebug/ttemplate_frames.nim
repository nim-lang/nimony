# Regression guard for nim-lang/nimony#1987: a template call must appear as an
# inlined stack frame, not as a jump into the template's definition.
#
# Before the fix, stepping over `setElem(p, 0, 'H')` in lldb landed on
# `system.nim:34` (the body of `[]=`) with a bare
#     !DILocation(line: 34, column: 10, scope: !<the caller>)
# and no `inlinedAt`, so the debugger had no frame to attribute it to.
#
# The golden below pins the shape that fixes it: one DISubprogram per expanded
# template, and every location inside it chained back to its call site through
# `inlinedAt`. `setElem` expands `[]=`, so the chain is two levels deep — that
# nesting is the part most likely to regress (building the call-site location
# after pushing the frame would silently flatten it).

template setElem(x: ptr UncheckedArray[char]; i: int; elem: char) =
  x[i] = elem

proc run(p: ptr UncheckedArray[char]) {.exportc.} =
  setElem(p, 0, 'H')
  setElem(p, 1, 'i')
