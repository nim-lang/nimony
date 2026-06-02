## Bare `typedesc` parameters (`T: typedesc`) become `(typekind (typedesc))` formals.

template test(T: typedesc) = discard

test int
test float32
