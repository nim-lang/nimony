import std/syncio

proc f1(a, i: int): bool = i != 5
proc main() =
  for i in 1..10:
    echo i
    block:
      let x = 12
      if not f1(x, i):
        break
  var j = 0
  while true:
    inc j
    block outer:
      if j == 2: break outer
      if j == 4: break
    echo "j ", j
  block named:
    while true:
      block: break named
    echo "unreachable"
  echo "done"
main()
