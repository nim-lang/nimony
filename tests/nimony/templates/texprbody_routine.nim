# A routine declared in a template whose body is an expression of an untyped
# parameter: the body is checked when the template is defined (and gets its
# `result` then) and again where it is expanded (nimony#2906, item 7).

import std/syncio

type F = proc (): string {.closure.}

template closureOf(v: untyped) =
  let f: F = proc (): string {.closure.} = $v
  echo f()

template namedProc(v: untyped) =
  proc g(): string = $v & "!"
  echo g()

template withStmts(v: untyped) =
  proc h(x: int): int =
    let y = x * 2
    y + v
  echo h(3)

template typedTail(v: untyped) =
  proc k(): int =
    result = v
    inc result
  echo k()

template inferredLambda(v: untyped) =
  # No annotation and no `{.closure.}`: `f`'s type is inferred from the
  # lambda. It was the lambda's declaration, which lost its name at the
  # expansion and crashed the backend.
  let f = proc (): string = "x" & v
  let g = proc (): string =
    result = v & "y"
  echo f(), g()

closureOf("a")
closureOf(12)
namedProc("b")
withStmts(1)
typedTail(4)
inferredLambda("-")
