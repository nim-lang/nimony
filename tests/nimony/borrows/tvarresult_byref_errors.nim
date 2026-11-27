# The first parameter of a `var T` result is passed by reference unless it is
# a `sink` or a view (#3187): those stay the callee's own, so nothing may be
# borrowed from them, not even through a callee that forwards its argument's
# storage: a `sink` parameter dies when the callee returns. The caller's own
# mutability rules still hold.

proc direct(a: array[1, int]): var int = a[0]
proc bySink(a: sink array[1, int]): var int = a[0]
proc sinkSeq(s: sink seq[int]): var int = s[0]
proc byView(x: openArray[int]): var openArray[int] = x
let la = [1]
direct(la) = 5
direct([2]) = 6
