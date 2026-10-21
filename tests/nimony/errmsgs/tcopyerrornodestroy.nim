# issue #2729: an .error =copy is honored without a =destroy
type Foo = object
proc `=copy`(dst: var Foo; src: Foo) {.error.}
var a = Foo()
var b = a
