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

# `{.dirty.}` template touching a private field, expanded inside a generic of
# the owner module and instantiated HERE. Legitimate: the code was written in
# the owner module. Regression guard for issue #1988's fix.
var bumpMe = createGeneric[int](1)
bumpPrivate(bumpMe)
discard readPrivate(bumpMe)
