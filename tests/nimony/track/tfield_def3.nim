import fields_helper
type Foo = object
  foo: int
  bar: int

proc main() =
  var foo = FieldsHelper()
  discard foo.foo
  #           ^def
  discard foo.foo
