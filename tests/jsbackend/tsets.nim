import std/syncio
import std/sets

var a = initHashSet[int]()
a.incl 1
a.incl 2
a.incl 3
a.incl 2

echo a.len
echo 2 in a
echo 5 in a

var b = initHashSet[int]()
b.incl 2
b.incl 3
b.incl 4

echo intersection(a, b).len   # {2, 3}

var u = a
u.incl b                      # union in place: {1, 2, 3, 4}
echo u.len

var d = a
d.excl b                      # difference in place: {1}
echo d.len
echo 1 in d

a.excl 1
echo 1 in a
echo a.len
