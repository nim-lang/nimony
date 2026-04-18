import std/syncio

proc main1() =
    var b = 10
    proc a(x: int) {.closure,passive.} =
        echo "main1: ", b+x
    a(10)
main1()