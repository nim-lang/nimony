import std/assertions
# issue #703

proc main() =
  for i in 0 ..< 1:
    # Compile
    while true: discard i

    # could not find symbol: i.0
    while i == 0: discard

var input = "start"
while input != "quit":
  if input == "":
    continue
  if input == "exit":
    break
  input = "exit"

assert input == "exit"

# #2794: a `let` in a toplevel `while` condition is a loop local, not a global
var x = 3
var sum = 0
while (let s = x; s > 0):
  sum += s
  dec x
assert sum == 6
