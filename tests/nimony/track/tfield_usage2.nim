type Foo = object
  foo: int
  bar: int

proc main() =
  var foo = Foo()
  discard foo.foo
              #^usages
  discard foo.foo
  discard foo.bar
