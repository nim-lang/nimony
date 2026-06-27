# Helper for `tfieldnumbering`. The shape matters:
#
# * `sum` is a *generic* proc that reads the fields of a *generic* object type
#   `Pair`, and it is declared *before* `Pair`. Semchecking `sum`'s signature
#   forces `Pair[int, int]` to be instantiated (and its fields numbered) before
#   `Pair`'s own declaration is reached.
# * `Decoy` sits between them and declares fields with the same names. With the
#   old global running counter for field syms this perturbed the count, so the
#   field read baked into `sum` (numbered early) diverged from the struct
#   emitted for `Pair`'s canonical declaration (numbered later) — and the C
#   backend rejected `p.a_0` against `struct Pair { a_2; }`.
#
# Fields must be numbered per owning type (`a.0`/`b.0` for `Pair`), independent
# of semcheck order, so the access and the struct always agree.

func sum*[T](dummy: T; p: Pair[int, int]): int =
  result = p.a + p.b

type
  Decoy {.used.} = object
    a, b: int

  Pair*[A, B] = object
    a*: A
    b*: B
