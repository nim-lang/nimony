## A `when` condition may inspect the type of a toplevel `let`/`var` declared
## before it. The local is deferred to the body phase, but its signature is
## resolved on demand for the condition. nim-lang/nimony#1974.

import std / syncio

let a: int = 5
when a is SomeInteger:
  echo "a-int"
else:
  echo "a-other"

var b: string = "hi"
when b is SomeInteger:
  echo "b-int"
else:
  echo "b-other"

# the local stays usable in the body phase, including inside the taken branch
when a is SomeInteger:
  echo a
