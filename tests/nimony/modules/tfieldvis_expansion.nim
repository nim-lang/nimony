import deps/mfieldvis

# An `untyped` template defers semchecking of its argument until after
# expansion. The argument is this module's code, naming a private field of
# another module's type, and must be rejected exactly as the direct form is.
# Regression test for issue #1988.
template pass(cond: untyped): untyped = cond

discard pass(Foo(public: 1, private: 2))

var foo = Foo(public: 1)
discard pass(foo.private)

discard pass(Generic[int](public: 1, private: 2))
