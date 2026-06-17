type Foo = object
  foo: int
  bar: int

proc main() =
  var foo = Foo()
  discard foo.foo
  #       ^def
  discard foo.foo
