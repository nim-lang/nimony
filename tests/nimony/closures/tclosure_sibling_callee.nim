# a nested proc that captures NOTHING but calls a capturing sibling still
# needs the environment forwarded to it (issue #2378)
import std/syncio

proc runIt(k: int) =
  proc isK(n: int): bool {.closure.} = n == k
  proc probe(n: int) {.closure.} =
    echo "result=", isK(n)
  probe(3)

proc transitive(k: int) =
  proc leaf(n: int): bool {.closure.} = n == k
  proc mid(n: int): bool {.closure.} = leaf(n)
  proc top(n: int): bool {.closure.} = mid(n)
  echo "transitive=", top(3)

proc asValue(k: int) =
  # the sibling is used as a VALUE, not called directly
  proc isK(n: int): bool {.closure.} = n == k
  proc probe(n: int) {.closure.} =
    let f: proc (n: int): bool {.closure.} = isK
    echo "asValue=", f(n)
  probe(3)

proc noCapture() =
  # neither sibling needs an environment: no env parameter must appear
  proc a(): int = 4
  proc b(): int = a() + 1
  echo "plain=", b()

proc main() =
  runIt(3)
  transitive(3)
  asValue(3)
  noCapture()

main()
