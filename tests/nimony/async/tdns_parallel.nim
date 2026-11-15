## #2868: concurrent first lookups through the process-wide resolver. The
## first `resolve` initialises the default resolver, which reads `/etc/hosts`
## through the ring. That read must not happen under an OS mutex: a second
## worker blocking its thread on the mutex while the holder is parked used to
## hang the pool. `localhost` keeps the test off the network.
import std/[syncio, parfor, dns]

const N = 10

proc main =
  var ips = default(array[N, string])
  for i in `||`(0, N-1, chunkSize = 1):
    try:
      ips[i] = resolve("localhost")
    except ErrorCode:
      ips[i] = "error"
  var ok = 0
  for i in 0 ..< N:
    if ips[i] == "127.0.0.1": inc ok
  echo "resolved ", ok, " of ", N

main()
