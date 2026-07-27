# Regression for the `x = f(x)` last-use bug (mover.nim isLastReadImpl AsgnS):
# a use of `a` that precedes `a = f(a)` is NOT a last use — the RHS `f(a)` is
# evaluated before the store and reads the old value. Previously the forward
# scan saw the LHS `== a` and concluded "redefinition -> last use" WITHOUT
# checking the RHS, so `use(a)` was sunk and `a` zeroed before `f(a)` ran.

proc use(x: sink string) = discard
proc combine(x: string): string = x

proc main() {.report: "lastuse".} =
  var a = "3"
  use(a)
  #[  ^notlastuse]#
  a = combine(a)      # RHS reads `a` -> the use above is NOT last
  use(a)
  #[  ^lastuse]#

main()
