## Control flow: if/elif/else, while, for-in-range, and case.
import std/syncio

proc classify(n: int): string =
  if n < 0: "negative"
  elif n == 0: "zero"
  else: "positive"

echo classify(-3)
echo classify(0)
echo classify(8)

var sum = 0
for i in 0 ..< 5:
  sum = sum + i
echo sum                  # 0+1+2+3+4 = 10

var k = 3
while k > 0:
  echo k
  k = k - 1

case 2
of 1: echo "one"
of 2: echo "two"
else: echo "many"
