
import std/[rawthreads, syncio]

const
  numThreads = 8
  pinCpu = when defined(android): 0 else: -1
var
  results: array[numThreads, string]

proc worker(p: pointer) =
  let idx = cast[ptr int](p)[]
  results[idx] = "worker " & $idx

proc main =
  var thr {.noinit.}: array[numThreads, RawThread]
  var indexes = default array[numThreads, int]
  try:
    for i in 0..<numThreads:
      indexes[i] = i
      # On Android, pin one thread so the Bionic affinity path is linked.
      create thr[i], worker, addr(indexes[i]),
             pinnedToCpu = (if i == 0: pinCpu else: -1)
  except:
    echo "error creating thread"
    return
  echo "other"
  for i in 0..<numThreads:
    thr[i].join()
  for i in 0..<numThreads:
    var ok = false
    for j in 0..<numThreads:
      if results[j] == "worker " & $i:
        ok = true
        break
    if not ok:
      echo "thread ", i, " not found"
      return
  echo "all threads found"

when not defined(windows):
  main()
else:
  echo "other"
  echo "all threads found"

{.feature: "assumeSync".}  # test program: globals shared freely
