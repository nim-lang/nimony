import deps/mgenericconverter

proc bar[T](x: Foo[T], y: T) = discard

bar(123, 456)
let foo: Foo[int] = 123
bar(123.0, 456.0)
let foo2: Foo[float] = 123.0
