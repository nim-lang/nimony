## Regression: `foo[N, T](...)` must instantiate the generic proc `foo`, not
## treat bare `foo` as the imported module symbol when `import deps/foo` brings
## both a module and a proc named `foo` into scope.
##
## Nimony currently rewrites `foo[N, T]` as a subscript on the module symbol and
## then fails the surrounding call with "cannot call expression of type auto".
import std/syncio
import deps/foo

let xs = foo[3, int]([1, 2, 3])
echo xs[0]
echo xs[2]
