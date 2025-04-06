import std/assertions
import std/setutils

type Foo = enum
  A, B, C, D, E, F

var s, s1: set[Foo]
s = {A..E}
s1 = {B..F}

# --- set symmetric difference ---

assert s -+- s == {}
assert s -+- s1 == {A, F}
s.toggle(A); assert s == {B..E}
s.toggle(A); assert s == {A..E}
s.toggle({B, C, D}); assert s == {A, E}
s.toggle({B, C, D}); assert s == {A..E}

# --- set complement ---

assert emptySet(Foo) == {}
assert emptySet(Foo).isEmpty()
assert not emptySet(Foo).isFull()
assert fullSet(Foo) == {A, B, C, D, E, F}
assert fullSet(Foo).isFull()
assert not fullSet(Foo).isEmpty()

assert complement({A, B, C}) == {D, E, F}
assert complement({D, E, F}) == {A, B, C}

# --- set element syntax sugar ---

assert s[A] == true
assert s[F] == false
s[A] = false; assert s[A] != true
s[F] = true; assert s[F] != false

# --- set element iterator ---

s = emptySet(Foo)
s1 = {A, D..F}

for foo in s1.items:
  s.incl(foo)
assert s == s1
