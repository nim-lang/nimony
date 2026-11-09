# A `case` that keeps an `(err)` where a branch belongs, as the argument of a
# call that fails to match: rendering the call for the mismatch message must
# not crash on the `(err)`.

type K = enum ka, kb

proc g(a: string; b: int): string = a

proc f(k: K): string =
  result = "x" & (case k
    elif true: "a"
    else: "b")
  result = g((case k
    of ka: "a"), "notint")
  result = g((case k
    of ka: "a"
    elif true: "c"
    else: "b"), "notint")
