import std/syncio

# A proc literal's type is its declaration; used as a type argument it must
# give one `seq` instance however often it is checked (llms#311).

type
  Setup = proc (x: int): int {.nimcall.}
  Cb = proc (x: int): int {.closure.}

let cl = proc (x: string): string = x & "!"
let s = @[cl]
echo s.len, " ", s[0]("a")

let fl = func (x: int): int = x + 1
echo @[fl][0](1)

let a: seq[Setup] = @[proc (x: int): int {.nimcall.} = x * 2]
echo a[0](3)

let b: seq[(string, Cb)] = @[("b", proc (x: int): int {.closure.} = x + 4)]
echo b[0][0], " ", b[0][1](1)

echo sizeof(typeof(cl)) == sizeof(pointer)

proc main() =
  # nearest neighbour: a named proc
  proc f(x: int): int = x - 1
  let g = f
  echo @[g][0](10)

main()
