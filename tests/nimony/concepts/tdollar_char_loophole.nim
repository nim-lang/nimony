## KNOWN BUG, documented rather than fixed: a type can satisfy a concept
## through a *generic* candidate whose own constraint it does not meet.
##
## `Stringable` (system.nim) requires `func $(x: Self): string`. The only `$`
## in `system` whose parameter can bind a `char` is
## `func $*[T: enum](x: T): string` — and `char` is not an enum. Concept
## checking accepts that candidate on shape alone, ignoring its `T: enum`
## constraint, so `char` is deemed `Stringable`. The lie surfaces only once the
## generic body is instantiated and real overload resolution runs, which
## reports the failure inside the *callee's* source rather than at the call
## site — see the `.msgs` file next to this test.
##
## The three blocks below are, in order: the control (a type with no candidate
## at all, correctly rejected at the call site — this is the diagnostic `char`
## should get), the real-world symptom, and the mechanism reduced to a single
## user-defined candidate so the fix has an unambiguous target.
##
## Found while porting `std/packedsets`: `PackedSet[char]` type-checks happily
## and then fails to instantiate its `$`. (`$` for `char` lives in `strutils`,
## not in `system`, so importing `strutils` is what actually makes a `char`
## stringifiable — but that is a *scope* question, independent of this bug.)
##
## When this is fixed, all three blocks must report the same shape of error in
## the same place, and the `.msgs` file has to be regenerated
## (`hastur --overwrite test <this file>`).

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
