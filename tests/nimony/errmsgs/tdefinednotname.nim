# `defined` takes a (dotted) name; any other operand used to crash nimsem.
let a = defined(1 + 2)
let b = defined((1 + 2).x)
