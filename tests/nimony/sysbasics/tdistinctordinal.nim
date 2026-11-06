# A `distinct` ordinal is still an `Ordinal` (Nim compat): `ord`, `low`,
# `high`, `succ`, `inc`, `case`, `set` and array indexes work on it. The
# comparison operators are declared per type, like in Nim, so it only
# compares (and iterates with `..`) once it borrows them (#2902).

import std/syncio

type
  Day = distinct int
  Slot = distinct range[0..3]

func `==`(a, b: Day): bool {.borrow.}
func `<=`(a, b: Day): bool {.borrow.}
func `<`(a, b: Day): bool {.borrow.}

var d = Day(3)
echo ord(d), " ", ord(succ(d)), " ", ord(low(Slot)), " ", ord(high(Slot))
inc d
echo d == Day(4), " ", d < Day(4), " ", d <= Day(4)

case d
of Day(4): echo "four"
else: echo "other"

var s: set[Slot] = {Slot(1), Slot(3)}
echo Slot(1) in s, " ", Slot(2) in s

var a: array[Slot, int]   # `default(array[Slot, int])` walks a distinct index
a[Slot(2)] = 7
echo a[Slot(2)], " ", a.len

for x in Day(1)..Day(3): echo int(x)
for x in countdown(Day(2), Day(1)): echo int(x)
