# A constant expression that fails to evaluate is reported once, by its most
# precise error, in place of the expression: pragma arguments and case labels.

{.error: zzz & qqq.}
{.passL: (if true: "-lm" else: yyy).}
{.error: "computed " & "message".}

let x = 3
case x
of (if true: 1 else: www): discard
else: discard

let s = "a"
case s
of (if true: "a" else: vvv): discard
else: discard
