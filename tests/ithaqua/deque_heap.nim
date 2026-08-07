import std/[syncio, deques, heapqueue]

# deque: ring-buffer behavior across both ends, growth past initial capacity
var d = initDeque[int](4)
var i = 1
while i <= 10:                   # force at least one grow
  d.addLast i
  i = i + 1
d.addFirst 0
echo d.len                       # 11
echo d.peekFirst, " ", d.peekLast  # 0 10
echo d.popFirst, " ", d.popLast    # 0 10
echo d[0], " ", d[d.len - 1]       # 1 9
d[0] = 100
echo d.popFirst                  # 100
echo d.contains(5), " ", d.contains(99)
echo $d                          # Deque[int] has `$` for Stringable elements

var sum = 0
for x in d.items: sum = sum + x
echo sum                         # 2+..+9 = 44
d.clear
echo d.len                       # 0

let d2 = toDeque([7, 8, 9])
echo d2.peekFirst, " ", d2.peekLast

# heapqueue: min-heap ordering out of arbitrary insertion order
var h = initHeapQueue[int]()
for x in [5, 1, 9, 3, 7, 2, 8]:
  h.push x
echo h.len                       # 7
echo h[0]                        # 1 (min at root)
var drained = ""
while h.len > 0:
  drained.add $h.pop
  drained.add " "
echo drained                     # 1 2 3 5 7 8 9

var h2 = toHeapQueue([4, 2, 6])
echo h2.find(6) >= 0, " ", h2.contains(3)
echo h2.replace(1)               # pops min 2, pushes 1
echo h2.pushpop(0)               # 0 smaller than min -> comes straight back
echo h2.pop, " ", h2.pop         # 1 4 remaining mins in order
