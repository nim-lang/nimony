## A type must not satisfy a concept through a *generic* candidate whose own
## constraint it does not meet.
##
## `Stringable` (system.nim) requires `func $(x: Self): string`. The only `$`
## in `system` whose parameter can bind a `char` is
## `func $*[T: enum](x: T): string` — and `char` is not an enum. Concept
## checking resolves the requirement like a call, so the candidate's `T: enum`
## is enforced and `char` is rejected at the call site, with the same
## diagnostic a type without any candidate gets.
##
## The three blocks below are, in order: the control (a type with no candidate
## at all), the real-world symptom, and the mechanism reduced to a single
## user-defined candidate. All three must report the same shape of error in
## the same place.
##
## Found while porting `std/packedsets`: `PackedSet[char]` used to type-check
## and then fail to instantiate its `$`. (`$` for `char` lives in `strutils`,
## not in `system`, so importing `strutils` is what actually makes a `char`
## stringifiable — but that is a *scope* question, independent of this test.)

type
  Unstringable = distinct int
    ## No `$` candidate in scope at all.

  Renderable = concept
    func renderIt(x: Self): string

func renderIt[T: enum](x: T): string = "<enum>"
  ## The *only* `renderIt`, and `char` does not satisfy its `T: enum`.

proc render[T: Stringable](x: T): string = $x

proc show[T: Renderable](x: T): string = renderIt(x)

block: # control: rejected at the call site, as it should be
  discard render(Unstringable(3))

block: # symptom: `char` passes `Stringable`, then explodes inside `render`
  discard render('c')

block: # mechanism: the candidate's own `T: enum` constraint is not enforced
       # while checking `Renderable`, only once `show` is instantiated
  discard show('c')
