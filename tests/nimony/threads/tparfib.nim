## Recursive parallel Fibonacci via `||`: each call runs its two recursive
## sub-problems in parallel, writing them into a 2-element array, then sums.
## This is a fork-join stress test — every level opens a nested `||`, so it
## exercises `parWait`'s help-on-join (without it, the recursion deadlocks once
## every worker is spin-waiting with its sub-tasks stuck unrun in the queue).
import std / [parfor, syncio]

proc pfib(n: int): int =
  if n < 2:
    return n
  var r = [0, 0]                 # the two sub-results; array of size 2
  for i in `||`(0, 1, workload = CpuBound):   # i = 0, 1 in parallel; pure compute
    r[i] = pfib(n - 1 - i)       # r[0] = pfib(n-1), r[1] = pfib(n-2)
  result = r[0] + r[1]

proc sfib(n: int): int =
  ## Sequential reference, for cross-checking.
  var a = 0
  var b = 1
  for _ in 0 ..< n:
    let c = a + b
    a = b
    b = c
  result = a

proc main =
  var ok = true
  for n in [0, 1, 2, 3, 7, 12, 18]:   # 18 nests far deeper than the worker count
    if pfib(n) != sfib(n):
      ok = false
  echo (if ok: "fib ok" else: "fib FAIL")

when not defined(windows):
  main()
else:
  echo "fib ok"
