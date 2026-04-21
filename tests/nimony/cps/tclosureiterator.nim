import std/syncio
proc a(): iterator(): int {.closure.}=
    var b = 4
    return iterator(): int {.closure.} =
        yield b
        b = 1
        return 2
var x = a()
var iter = x()
while true:
    echo iter[0]
    if iter[1].fn == nil:
        break
    iter[1].complete()