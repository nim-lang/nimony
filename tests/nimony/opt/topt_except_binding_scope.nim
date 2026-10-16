import std/syncio

# #2675: a void raising call keeps its `canRaise` temp in a block of its own,
# and the code reaches the handler as `e = canRaise; jmp exlab`. Copy
# propagation must not turn `e` in the handler into a temp that is out of scope.

proc f() {.raises, noinline.} = raise IOError

proc run(): ErrorCode =
  result = Success
  try: f()
  except ErrorCode as e: result = e

echo run()
