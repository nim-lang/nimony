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

proc a2(x: int): proc (q: int) {.passive,closure.} =
    var b = x;
    return proc (q: int) {.passive,closure.} =
        b += q
        echo "a2 ", b

proc main3() =
    var b = 0
    let x = a2(10)
    x(10)
    x(10)

main3()

proc a3(x: int): proc (q: int): int {.passive,closure.} =
    var b = x;
    return proc (q: int): int {.passive,closure.} =
        b += q
        return b

proc main4() =
    let x = a3(10)
    var b = x(10)
    echo "main4: ", b
    b = x(10)
    echo "main4: ", b

main4()


proc a4(x: int) {.passive.} =
    echo "a4: ", x

proc a5(x: int) {.passive.} =
    echo "a5: ", x

proc main5() =
    var x = a4
    x(5)
    x = a5
    x(42)

main5()

proc main6() {.passive.} =
    let x = a3(10)
    var b = x(10)
    echo "main6: ", b
    b = x(10)
    echo "main6: ", b
    b = x(10)
    echo "main6: ", b

main6()

proc main7() {.passive.} =
    var b = 0
    let x = a()
    x()
    x()

main7()

proc main8() {.passive.} =
    var b = 10
    proc a(x: int) {.closure,passive.} =
        echo "main8: ", b+x
    a(10)
main8()

proc main9() {.passive.} =
    var b = 0
    let x = a2(10)
    x(10)
    x(10)

main9()

proc main10() {.passive.} =
    var x = a4
    x(5)
    x = a5
    x(42)

main10()