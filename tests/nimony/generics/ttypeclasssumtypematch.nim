import std/assertions

type
  ConceptA = concept
    proc `+`(a, b: Self): Self
    proc `*`(a, b: Self): Self

  ConceptB = concept
    proc `-`(a, b: Self): Self
    proc `*`(a, b: Self): Self

  BoxA = object
    v: int

  BoxB = object
    v: int

  BoxAB = object
    v: int

func `+`(a, b: BoxA): BoxA = BoxA(v: a.v + b.v)
func `*`(a, b: BoxA): BoxA = BoxA(v: a.v * b.v)

func `-`(a, b: BoxB): BoxB = BoxB(v: a.v - b.v)
func `*`(a, b: BoxB): BoxB = BoxB(v: a.v * b.v)

func `+`(a, b: BoxAB): BoxAB = BoxAB(v: a.v + b.v)
func `-`(a, b: BoxAB): BoxAB = BoxAB(v: a.v - b.v)
func `*`(a, b: BoxAB): BoxAB = BoxAB(v: a.v * b.v)

# Sum types must compile and run successfully

func allOpsAandB[T: ConceptA and ConceptB](a, b: T): T =
  (a + b) * (a - b)

func mulAorB[T: ConceptA or ConceptB](a, b: T): T =
  a * b

assert allOpsAandB(BoxAB(v: 1), BoxAB(v: 2)).v == -3
assert mulAorB(BoxA(v: 1), BoxA(v: 2)).v == 2
assert mulAorB(BoxB(v: 1), BoxB(v: 2)).v == 2
