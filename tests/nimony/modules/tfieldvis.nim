import deps/mfieldvis

var foo = Foo(public: 123)
discard getPrivate(foo)
discard getPrivateTempl(foo)

var generic = Generic[int](public: 123)
discard getPrivate(foo)
discard getPrivateTempl(foo)

template resem() =
  foo = Foo(public: 123)
  generic = Generic[int](public: 123)
resem()
