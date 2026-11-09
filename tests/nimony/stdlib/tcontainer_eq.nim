# `==` on Deque and HashSet compares contents, not internal layout.
import std/[syncio, deques, sets]
import mcontainer_eq

# Deque: same values, other head position
var a = initDeque[int]()
a.addLast 1
a.addLast 2
var b = initDeque[int]()
b.addFirst 2
b.addFirst 1
echo "deque reordered layout: ", a == b
# wraparound: four slots, head moved by pops
var c = initDeque[int](4)
for i in 0 ..< 4: c.addLast i
discard c.popFirst()
discard c.popFirst()
c.addLast 4
c.addLast 5
echo "deque wrapped: ", c == toDeque([2, 3, 4, 5])
echo "deque grown vs small: ", toDeque([2, 3, 4, 5]) == c
echo "deque other order: ", toDeque([1, 2]) == toDeque([2, 1])
echo "deque prefix: ", toDeque([1, 2]) == toDeque([1, 2, 3])
echo "deque empty: ", initDeque[int]() == initDeque[int](16)
echo "deque strings: ", toDeque(["x", "yy"]) == toDeque(["x", "yy"]), " ",
  toDeque(["x", "yy"]) == toDeque(["x", "y"])
echo "deque !=: ", a != b, " ", toDeque([1]) != toDeque([2])

# HashSet: insertion order, excluded members, size
var s = initHashSet[int]()
s.incl 1
s.incl 2
var u = initHashSet[int]()
u.incl 2
u.incl 1
var v = initHashSet[int]()
v.incl 1
v.incl 2
v.incl 3
v.excl 3
echo "set reordered: ", s == u
echo "set after excl: ", s == v, " ", v == s
var w = initHashSet[int]()
w.incl 1
echo "set subset: ", w == s, " ", s == w
var x = initHashSet[int]()
x.incl 1
x.incl 3
echo "set same size other member: ", s == x
var big1 = initHashSet[string]()
var big2 = initHashSet[string]()
for i in 0 ..< 20: big1.incl $i
for i in countdown(19, 0): big2.incl $i
echo "set hashed strings: ", big1 == big2
big2.excl "7"
echo "set hashed strings minus one: ", big1 == big2

# generic code in a module that imports neither container, templates
echo "generic: ", same(a, b), " ", same(s, v)
echo "template: ", tsame(a, b), " ", tsame(s, u)
proc localGeneric[T: Equatable](x, y: T): bool = x == y
echo "local generic: ", localGeneric(a, b), " ", localGeneric(s, v)

# containers nested in objects, tuples and seqs use the containers' `==`
type Holder = object
  d: Deque[int]
  s: HashSet[int]
echo "object field: ", Holder(d: a, s: s) == Holder(d: b, s: v)
echo "tuple field: ", (a, s) == (b, v)
echo "seq of sets: ", @[s] == @[v]
