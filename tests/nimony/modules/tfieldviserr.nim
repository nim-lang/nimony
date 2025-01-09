import deps/mfieldvis

var foo = Foo(private: 123)
discard foo.private

var generic = Generic[int](private: 123)
discard generic.private
