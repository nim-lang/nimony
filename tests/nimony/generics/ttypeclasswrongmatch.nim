import deps/mtypeclassmatch

foo2(123)
foo3(e0)

proc foo1All[T: ConceptA](x: T) =
  if false:
    foo1(x) # works
    foo2(x) # fails as intended
    foo3(x) # XXX matches, might cause problems for overloading
    foo4(x) # works

proc foo2All[T: ConceptA and ConceptB](x: T) =
  if false:
    foo1(x) # works
    foo2(x) # works
    foo3(x) # fails as intended
    foo4(x) # works

proc foo3All[T: ConceptA and not ConceptB](x: T) =
  if false:
    foo1(x) # works
    foo2(x) # fails as intended
    foo3(x) # works
    foo4(x) # works

proc foo4All[T: ConceptA or ConceptB](x: T) =
  if false:
    foo1(x) # fails as intended
    foo2(x) # fails as intended
    foo3(x) # fails as intended
    foo4(x) # works
