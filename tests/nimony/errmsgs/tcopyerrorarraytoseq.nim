# `@` copies at the call site, where an `.error` `=copy` is reported
type Foo = object
proc `=copy`(dst: var Foo; src: Foo) {.error.}
var a = Foo()
var s = @[a]
discard a
