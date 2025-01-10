import deps/mfieldvis

var foo = Foo(private: 123)
discard foo.private

var generic = Generic[int](private: 123)
discard generic.private

when false: # compiles and should compile but cannot test yet
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
