import std/syncio

# A void template whose body is a `when` that compiles to nothing publishes an
# empty `(stmts)`; its expansion was typed `auto` and rejected where `void` was
# wanted ("type mismatch: got: auto but wanted: void").

const Check = false

template step(what: string) =
  when Check:
    echo what

template stepElse(what: string) =
  when Check:
    echo what
  else:
    discard

proc f(): int =
  step("x")
  stepElse("y")
  result = 1

echo f()
