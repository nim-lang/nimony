## Regression: `foo[R, C, T](...)` must instantiate the generic proc `foo`, not
## treat bare `foo` as the imported module symbol when `import deps/foo` brings
## both a module and overloaded procs named `foo` into scope (nim-lang/nimony#2130).
import std/syncio
import deps/foo

let fromArray = foo[3, 1, int]([1, 2, 3])
echo fromArray[0]
echo fromArray[2]

let fromFill = foo[2, 2, int](7)
echo fromFill[1]
echo fromFill[3]
