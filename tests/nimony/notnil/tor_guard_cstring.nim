# The bug3 repro, made self-contained and runnable: a `if a == nil or b == nil:
# return` guard must establish `a != nil` AND `b != nil` on the fall-through, so
# the non-nil `cstring` params of `use` are satisfied. The `or` short-circuits to
# jmp/lab, whose facts survive the label join (see contracts_fir's flow tracker).

proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

proc getA(): nil cstring = cstring"hello"
proc getB(): nil cstring = cstring"world"

proc use(a, b: cstring) =
  printf("%s %s\n", a, b)

proc bug() =
  let a = getA()
  let b = getB()
  if a == nil or b == nil:
    return
  use(a, b)   # both a and b are proven non-nil here

bug()
