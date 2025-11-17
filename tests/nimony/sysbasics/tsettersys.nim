type Foo = object

proc `bar=`(x: Foo; y: int) = discard
proc `[]=`(x: Foo; y: int; z: int) = discard

var foo: Foo
foo.bar = 123
foo[123] = 456
