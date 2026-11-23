import deps/mfieldvis

# A private field of an imported type set through an untyped template is
# rejected as in the direct constructor, also when a proc of the field's name
# is in scope (the template binds the key to it).
type Other = object
proc private(o: Other): int = 0

template make(T: untyped) {.untyped.} =
  discard T(public: 1, private: 2)

make(Foo)
