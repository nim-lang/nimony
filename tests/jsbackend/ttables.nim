import std/syncio
import std/tables

var t = initTable[string, int]()
t["one"] = 1
t["two"] = 2
t["three"] = 3
t["two"] = 22

echo t.len
echo t.getOrDefault("one", -1)
echo t.getOrDefault("two", -1)
echo "three" in t
echo "four" in t
echo t.getOrDefault("four", -1)

var total = 0
for k, v in t:
  total = total + v
echo total
