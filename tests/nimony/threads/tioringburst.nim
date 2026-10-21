# More ops in flight at once than a lane's op queue starts out with: the queue
# grows, every op gets its own slot, and every one of them completes once.
import std / [ioring, syncio]

const Burst = 3000

var results = newSeq[int](Burst)
for i in 0 ..< Burst:
  results[i] = -1
  discard submitTimeout(afterMs(1), resPtr = addr results[i])

var comps: array[64, IoCompletion]
var got = 0
while got < Burst:
  got += waitCompletions(comps)

var ok = 0
for r in results:
  if r == 0: inc ok
echo "completed=", got, " ok=", ok
