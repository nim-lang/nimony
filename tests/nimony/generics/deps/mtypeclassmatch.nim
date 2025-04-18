when false: # XXX use when concept matching is implemented
  type
    ConceptA* = concept
      proc `+`(a, b: Self): bool
    ConceptB* = concept
      proc `-`(a, b: Self): bool
else:
  type
    ConceptA* = Ordinal
    ConceptB* = (enum) # orthogonal with Ordinal, also implemented as `or` type

proc foo1*[T: ConceptA](x: T) =
  if false:
    foo1(x)
    #foo2(x)
    #foo3(x)
    foo4(x)

proc foo2*[T: ConceptA and ConceptB](x: T) =
  if false:
    foo1(x)
    foo2(x)
    #foo3(x)
    foo4(x)

proc foo3*[T: ConceptA and not ConceptB](x: T) =
  if false:
    foo1(x)
    #foo2(x)
    foo3(x)
    foo4(x)

proc foo4*[T: ConceptA or ConceptB](x: T) =
  if false:
    #foo1(x)
    #foo2(x)
    #foo3(x)
    foo4(x)

type EnumA* = enum e0, e1, e2
