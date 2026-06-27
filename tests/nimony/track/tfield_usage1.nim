type Foo = object
  foo: int
  #^usages
  bar: int

proc main() =
  var foo = Foo()
  discard foo.foo
  discard foo.foo
  discard foo.bar
