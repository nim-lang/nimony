# RFC #575: `owned ref T` / `owned proc` on top of reference counting.
{.feature: "ownedRefs".}
import std/syncio

type
  Node = ref object
    next: owned nil Node
    data: int
  Cb = owned proc (x: int): int {.closure.}

proc sum(list: nil Node): int =
  result = 0
  var it = list
  while it != nil:
    result += it.data
    it = it.next

proc build(n: int): owned nil Node =
  result = nil
  for i in 1..n:
    let x = Node(data: i, next: move result)
    result = x

proc keep(s: var seq[Node]; x: sink Node) = s.add x

proc ident[T](x: owned T): owned T = x

proc mk(k: int): Cb =
  result = proc (x: int): int {.closure.} = x + k

proc consume(cb: Cb): int = cb(10)

proc main =
  var root = build(5)
  echo sum(root)
  let u: nil Node = root   # owned -> unowned is a counted reference
  root = nil               # the owner dies, `u` keeps the list alive
  if u != nil:
    echo u.data, " ", sum(u)
  var s: seq[nil Node] = @[]
  s.add build(3)
  echo sum(s[0])
  var os: seq[owned nil Node] = @[]
  os.add build(2)
  echo sum(os[0])

  var store: seq[Node] = @[]
  let a = Node(data: 7)
  keep(store, a)       # not the last read: an unowned copy is passed
  echo a.data, " ", store[0].data
  echo ident(Node(data: 8)).data
  let f = mk(3)
  echo f(4)
  let g: proc (x: int): int {.closure.} = f
  echo g(-2)
  let h = mk(5)
  echo consume(h)      # the last read of `h`: moved into the owned parameter

main()
