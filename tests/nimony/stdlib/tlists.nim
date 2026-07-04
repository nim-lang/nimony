import std/assertions
import std/lists

# ===================== SinglyLinkedList =====================
block:
  var L = initSinglyLinkedList[int]()
  assert $L == "[]"
  assert not L.contains(5)
  assert L.find(5) == nil

  L.append(1)
  L.append(2)
  L.append(3)
  L.prepend(0)
  assert $L == "[0, 1, 2, 3]"
  assert L.contains(2)
  assert not L.contains(9)
  assert L.find(3) != nil
  assert L.find(3).value == 3

  # items iterator
  var sum = 0
  for x in L.items: sum += x
  assert sum == 6

  # nodes iterator
  var cnt = 0
  for n in L.nodes: inc cnt
  assert cnt == 4

  # remove head
  assert L.remove(L.find(0))
  assert $L == "[1, 2, 3]"
  # remove middle
  assert L.remove(L.find(2))
  assert $L == "[1, 3]"
  # remove tail, then append still works (tail pointer valid)
  assert L.remove(L.find(3))
  assert $L == "[1]"
  L.append(9)
  assert $L == "[1, 9]"
  # remove down to empty
  assert L.remove(L.find(1))
  assert L.remove(L.find(9))
  assert $L == "[]"
  L.append(42)
  assert $L == "[42]"

  # value semantics: copy is independent
  var A = initSinglyLinkedList[int]()
  A.append(1); A.append(2); A.append(3)
  var B = A            # =dup deep copy
  B.append(4)
  assert $A == "[1, 2, 3]"
  assert $B == "[1, 2, 3, 4]"
  var C = initSinglyLinkedList[int]()
  C.append(99)
  C = A                # =copy deep copy
  C.append(7)
  assert $A == "[1, 2, 3]"
  assert $C == "[1, 2, 3, 7]"

# string elements ($ via Stringable)
block:
  var S = initSinglyLinkedList[string]()
  S.append("a"); S.append("b"); S.prepend("z")
  assert $S == "[z, a, b]"
  assert S.contains("a")

# ===================== DoublyLinkedList =====================
block:
  var L = initDoublyLinkedList[int]()
  assert $L == "[]"
  L.append(1); L.append(2); L.append(3)
  L.prepend(0)
  assert $L == "[0, 1, 2, 3]"
  assert L.contains(3)
  assert L.find(2).value == 2

  # prev links correct: tail walks back
  assert L.tail.value == 3
  assert L.tail.prev.value == 2
  assert L.head.next.prev.value == 0

  var sum = 0
  for x in L.items: sum += x
  assert sum == 6

  # remove head / middle / tail (O(1))
  assert L.remove(L.find(0))
  assert $L == "[1, 2, 3]"
  assert L.head.prev == nil
  assert L.remove(L.find(2))
  assert $L == "[1, 3]"
  assert L.head.next.value == 3
  assert L.tail.prev.value == 1
  assert L.remove(L.find(3))
  assert $L == "[1]"
  L.append(5)
  assert $L == "[1, 5]"
  assert L.tail.prev.value == 1
  assert L.remove(L.find(1))
  assert L.remove(L.find(5))
  assert $L == "[]"
  L.append(8)
  assert $L == "[8]"

  # value semantics
  var A = initDoublyLinkedList[int]()
  A.append(1); A.append(2)
  var B = A
  B.append(3)
  assert $A == "[1, 2]"
  assert $B == "[1, 2, 3]"
  # deep copy has its own prev links
  assert B.tail.prev.value == 2
