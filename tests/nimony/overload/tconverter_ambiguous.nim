# Each candidate needs one converter: still ambiguous, and each candidate
# is listed once (planetis-m/nimony-bugs-llms#331).
type A = object
type B = object
converter toA(x: int): A = A()
converter toB(x: int): B = B()
proc h(a: A): int = 1
proc h(b: B): int = 2
discard h(3)
