import deps/mfieldvis

var foo = Foo(public: 123)
discard getPrivate(foo)
discard getPrivateTempl(foo)

var generic = Generic[int](public: 123)
discard getPrivate(foo)
discard getPrivateTempl(foo)
generic = createGeneric[int](456)

template resem() =
  foo = Foo(public: 123)
  generic = Generic[int](public: 123)
  discard getPrivateTempl(generic)
  generic = createGeneric[int](456)
resem()

proc scope() =
  let differentGeneric = createGeneric[float](123)
