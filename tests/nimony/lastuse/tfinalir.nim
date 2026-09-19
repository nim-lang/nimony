# The Final IR keeps `case` and `try` as structured statements — the goto form
# the analysis used to run on flattened both away. These pin that the walk
# forks into every `case` branch and that a `finally` is on the path out of the
# protected body. `earlyreturn` pins that a `return` ends its path: the code
# after the enclosing `if` is not reachable from inside the returned expression.

proc use(x: sink string) = discard

proc incase(x: int) {.report: "lastuse".} =
  # one branch reads `a` again
  var a = "3"
  use(a)
  #[  ^notlastuse]#
  case x
  of 0: use(a)
  of 1: a = "4"
  else: discard

proc caseAllRedefine(x: int) {.report: "lastuse".} =
  # every branch overwrites `a` unread
  var a = "3"
  use(a)
  #[  ^lastuse]#
  case x
  of 0: a = "4"
  of 1: a = "5"
  else: a = "6"
  use(a)

proc infinally() {.report: "lastuse".} =
  # the cleanup reads `a` on the way out of the protected body
  var a = "3"
  try:
    use(a)
    #[  ^notlastuse]#
  finally:
    use(a)

proc earlyreturn(x: int): string {.report: "lastuse".} =
  var a = "3"
  if x == 0:
    return a
    #[     ^lastuse]#
  a.add "4"
  result = a

incase(0)
caseAllRedefine(0)
infinally()
discard earlyreturn(0)
