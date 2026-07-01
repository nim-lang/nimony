import std/assertions
import std/lists

# --- empty list ---
var L = initSinglyLinkedList[int]()
assert $L == "[]"
assert not L.contains(5)
assert L.find(5) == nil

# --- append / prepend ordering ---
L.append(1)
L.append(2)
L.append(3)
L.prepend(0)
assert $L == "[0, 1, 2, 3]"
assert L.contains(0)
assert L.contains(3)
assert not L.contains(9)

# --- add alias (== append) ---
L.add(4)
assert $L == "[0, 1, 2, 3, 4]"

# --- find ---
let n2 = L.find(2)
assert n2 != nil
assert n2.value == 2
assert L.find(99) == nil

# --- remove middle ---
assert L.remove(n2) == true
assert $L == "[0, 1, 3, 4]"
assert not L.contains(2)
assert L.remove(n2) == false        # already removed

# --- remove head ---
let nh = L.find(0)
assert L.remove(nh) == true
assert $L == "[1, 3, 4]"

# --- remove tail, then append must still work (tail pointer maintained) ---
let nt = L.find(4)
assert L.remove(nt) == true
assert $L == "[1, 3]"
L.append(5)
assert $L == "[1, 3, 5]"

# --- nodes iterator ---
var count = 0
var sum = 0
for node in L.nodes:
  count = count + 1
  sum = sum + node.value
assert count == 3
assert sum == 9

# --- remove down to empty resets tail; re-append works ---
var L2 = initSinglyLinkedList[int]()
L2.append(7)
let only = L2.find(7)
assert L2.remove(only) == true
assert $L2 == "[]"
L2.append(8)
assert $L2 == "[8]"

# --- string element type ---
var S = initSinglyLinkedList[string]()
S.append("a")
S.prepend("b")
assert $S == "[b, a]"
assert S.contains("a")
assert not S.contains("z")
