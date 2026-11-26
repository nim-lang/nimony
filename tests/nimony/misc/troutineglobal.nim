# A `var x {.global.}` in a routine is one location for the whole program:
# sem moves it to module scope under a module-unique name.
# - #3167: inlining `counter` gave every inlined call its own copy; with
#   `result = calls` the inliner also folded the global into a temporary.
# - #3158: same-named globals of two modules (and of a template's expansions
#   in two modules) were one C symbol.
# - #3159: a global (or thread-local) without an initializer was reset on
#   every call.

when defined(nimony):
  {.feature: "assumeSync".}
import std/syncio
import deps / [mroutineglobala, mroutineglobalb]

proc counter(): int =
  var calls {.global.}: int
  inc calls
  result = calls * 1

proc counterDirect(): int =
  var calls {.global.}: int
  inc calls
  result = calls

# one global per instance
proc gen[T](x: T): int =
  var n {.global.}: int
  inc n
  result = n

# the initializer runs once
var inits = 0
proc startVal(): int =
  inc inits
  result = 100

proc withInit(): int =
  var v {.global.} = startVal()
  inc v
  result = v

proc remember(x: int): int {.noinline.} =
  var seen {.global.}: seq[int]
  seen.add x
  result = seen.len

proc rememberInit(x: int): int {.noinline.} =
  var seen {.global.} = newSeqOfCap[int](4)
  seen.add x
  result = seen.len

proc rememberThread(x: int): int {.noinline.} =
  var seen {.threadvar.}: seq[int]
  seen.add x
  result = seen.len

# hoisted before xelim, so a complex initializer moves along with it
var flag = true
proc ifInit(): int =
  var v {.global.} = (if flag: 10 else: 20)
  inc v
  result = v

proc main() =
  echo counter(), " ", counter(), " ", counter()
  echo counterDirect(), " ", counterDirect(), " ", counterDirect()
  echo gen(1), " ", gen(2), " ", gen("a"), " ", gen("b")
  echo withInit(), " ", withInit(), " inits=", inits
  discard fromA()
  discard fromA()
  echo fromB()
  echo a1(), " ", a1(), " ", a1(), "  b: ", b1(), " ", b1(), " ", b1()
  for i in 1 .. 3:
    echo remember(i), " ", rememberInit(i), " ", rememberThread(i)
  echo ifInit(), " ", ifInit()

main()
