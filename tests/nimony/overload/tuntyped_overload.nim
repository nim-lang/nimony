# An argument that does not resolve picks the overload with an untyped
# parameter; the overloads with typed parameters do not report it.
# (Nim 2: nim-lang/Nim#14827 and the "Lazy type resolution for untyped" family.)

import std/syncio

template fun(a: bool, body: untyped) = echo "bool, untyped"
template fun(a: int, body: untyped) = echo "int, untyped"
template fun(body: untyped) = echo "untyped"

fun(true, nonexistent)
fun(1, nonexistent)
fun(nonexistent)

# Same arity, either declaration order.
template g(a: int) = echo "g int"
template g(a: untyped) = echo "g untyped"

template h(a: untyped) = echo "h untyped"
template h(a: int) = echo "h int"

g(undeclared)
h(undeclared)

# The arity mismatch shows only at a later argument (nim-lang/Nim#23662).
template t(n: float) = echo "t float"
template t(todecl: untyped, x: int) = echo "t untyped, int"
t(ii, 3)

# A body that uses an injected symbol (nim-lang/Nim#19556).
template foo(a: string, body: untyped) =
  block:
    var fooz {.inject.} = "foo1"
    body

template foo(body: untyped) =
  block:
    var fooz {.inject.} = "foo2"
    body

foo("aaa"):
  echo fooz
foo:
  echo fooz

# An iterator call as the argument (nim-lang/Nim#9219).
template it(a: int) = echo "it int"
template it(a: untyped) = echo "it untyped"
iterator myiter(): int = yield 0
it(myiter())
