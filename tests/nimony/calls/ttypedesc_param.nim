## Bare `typedesc` parameters (`T: typedesc`) become `(typekind (typedesc))` formals.

import std/syncio

template test(T: typedesc) =
  when T is float32: # match built-in type
    const a = 1.2 # local constant and literal
    echo "float ", a
  elif T is SomeInteger: # match union type
    const a = 12.T # local generic constant and literal
    echo "integer ", a
  else:
    discard

test float32
test int32
