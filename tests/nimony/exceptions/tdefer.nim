
import std / syncio

proc test(a: string) =
  defer:
    echo a

test("ac")

proc testC(a: string) =
  if a.len > 0:
    defer:
      echo "b"

testC("")

proc testD(a: string) =
  echo "begin", a
  defer:
    echo "end"
  echo "middle"

testD("")

proc testB(a: string) =
  defer:
    echo a
  defer:
    echo "b"

testB("ac")

# bug #1440

proc f(n: int): int =
  var n = n
  defer:
    inc n
    echo n
  return n

echo f(5)

# regression: an exit path BEFORE the defer must not run the defer body
# (the lowering used to wrap the whole scope in the try/finally, so an
# early raise ran the defer and its generated code referenced locals
# that are not declared on that path)

proc g(x: int): int {.raises.} =
  if x < 0:
    raise BadOperation
  var v: seq[int] = @[]
  defer:
    echo "release ", v.len
  v.add x
  result = v.len

try:
  echo g(3)
except:
  echo "caught"

try:
  echo g(-1)
except:
  echo "caught"

# a branch scope inside a `defer` body must not be mistaken for the enclosing
# block: the block's first `defer` was wrapped around the `if c` branch only
proc branchInDefer(c: bool) =
  var v0 = 0
  var v1 = 1
  discard 0
  block:
    defer:
      echo "outer defer"
    defer:
      if c:
        echo "c"
      else:
        echo "not c"

branchInDefer(true)
branchInDefer(false)

# a `defer` directly inside a `defer` body
proc deferInDefer() =
  var x = 0
  discard 0
  defer:
    echo "outer"
  defer:
    defer:
      echo "nested"
    echo "body"

deferInDefer()

# a `defer` in an expression's branch: the branch is a scope, its `defer` runs
# when the branch ends, after the branch's value has been computed
proc deferInExprBranch(v: int) =
  var x = 1
  let a = if v > 0:
      defer:
        x = 5
        echo "leaving branch"
      x
    else:
      0
  echo a, " ", x

deferInExprBranch(1)

# `(;)` is no scope: its `defer` runs when the enclosing scope exits. Here that
# scope is the branch or the block, which ends right after the `(;)`
proc deferInStmtListExpr(v: int) =
  let a = if v > 0:
      (echo "s"; defer: echo "d"; v)
    else:
      0
  echo "after ", a
  let b = if v > 0:
      (echo "p"; defer: echo "d"; echo "y"; v)
    else:
      0
  echo "after ", b
  let c = block:
    (echo "p"; defer: echo "d"; echo "y"; v + 1)
  echo "after ", c

deferInStmtListExpr(2)
