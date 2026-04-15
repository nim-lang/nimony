import std/syncio

proc a(x: var int): int {.passive.} =
  inc x
  return x

proc main() {.passive.}=
    var i = 0
    var k = 0
    while a(i) < 10:
        while a(k) < 10:
            echo "inner"
        echo "outer"

main()