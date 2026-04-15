import std/syncio

proc main1() =
    var b = 10
    proc a(x: int) {.closure,passive.} =
        echo "main1: ", b+x
    a(10)
main1()

proc a(): proc () {.passive,closure.} =
    var b = 0;
    return proc () {.passive,closure.} =
        inc b
        echo "X ", b

proc main2() =
    var b = 0
    let x = a()
    x()
    x()

main2()