import std / syncio

type
  Callback = proc (a, b: int)

proc theProc(a, b: int) =
  echo a, " ", b

proc main =
  var c: Callback = theProc
  c(1, 3)

main()

# a proc literal's type is a proc type, not its declaration
# (nimony-bugs-llms#332)
type Setup = proc (x: int): int {.closure.}

proc literals =
  var cases: seq[(string, Setup)] = @[]
  cases.add ("a", proc (x: int): int {.closure.} = x + 1)
  let t = ("b", proc (x: int): int = x + 2)
  let it = iterator (): int {.closure.} =
    yield 3
  for v in it(): echo cases[0][1](0), " ", t[1](0), " ", v

literals()
