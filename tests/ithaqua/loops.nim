import std/syncio

# while-loop accumulation: sum 1..1000 = 500500
var s = 0
var i = 1
while i <= 1000:
  s = s + i
  i = i + 1
echo s

# nested loops: sum of x*y over 1..10 x 1..10 = 3025
var total = 0
var x = 1
while x <= 10:
  var y = 1
  while y <= 10:
    total = total + x * y
    y = y + 1
  x = x + 1
echo total

# break out of an unbounded loop.
var n = 0
while true:
  n = n + 1
  if n >= 5:
    break
echo n
