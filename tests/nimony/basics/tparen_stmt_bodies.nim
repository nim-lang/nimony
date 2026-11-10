# A statement in parentheses keeps a lone body: `(for x in a: s)` holds `s`,
# not a statement list. Each body below binds a call operand to a temp, so it
# lowers to several statements in a slot that holds one.
import std/syncio

proc f(x: int): int = x + 1
proc g(x: int): int = x * 2

proc branches(k: int): int =
  var r = 0
  (if k == 0: r = f(g(k + 10)) elif k == 1: r = f(g(k + 20)) else: r = f(g(k + 30)))
  r

proc cases(k: int): int =
  var r = 0
  (case k
   of 0: r = f(g(k + 1))
   of 1: r = f(g(k + 2))
   else: r = f(g(k + 3)))
  r

proc gen[T](x: seq[T]): string =
  var r = ""
  (for y in x: r = r & $f(g(x.len)))
  r

template twice(body: untyped) =
  (for i in 0..1: body)

var total = 0
if true: (for x in [1, 2]: total += x)
echo total

var s = ""
(for x in [1, 2]: s = s & $f(g(x)))
echo s

var a = [1, 2]
(for x in mitems(a): x = f(g(x)))
echo a[0], " ", a[1]

total = 0
(for x in [1, 2]: (for y in [10, 20]: total += f(g(x + y))))
echo total

total = 0
(block: total = f(g(total)))
echo total

for k in 0..2: echo branches(k)
for k in 0..2: echo cases(k)

var w = "a"
(while w.len < 4: w = w & $f(g(w.len)))
echo w

var i = 0
(while true: (if i >= 3: break else: i = f(g(i))))
echo i

total = 0
(try: total = f(g(5)) finally: total = f(g(total)))
echo total

total = 0
(try: total = f(g(7)) except: total = 0)
echo total

echo gen(@[1, 2])
echo gen(@["a"])

var t = 0
twice(inc(t, f(g(t))))
echo t
