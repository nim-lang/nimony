# Regression: inherited concept errors must list missing procs even when
# matchConceptBody cached a parent failure with an empty missing list.

type
  Q = concept
    proc foo(x: Self)

  P = concept of Q
    proc bar(x: Self)

  C = concept of P

type X = object
  y: int

type Box[T: C] = object
  v: T

var x: Box[X]
