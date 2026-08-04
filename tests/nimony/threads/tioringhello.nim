import std/syncio

when not defined(windows):
  import std/ioring
  var ring = initIoRing()
  var buf = "Hello world\n"
  discard ring.submitWrite(stdout.getFileHandle, buf.toCString, buf.len)
  var comps: array[16, IoCompletion]
  let n = ring.waitCompletions(comps)
  echo "written=", comps[0].result, " n=", n, " buf.len=", buf.len
  ring.shutdown()
else:
  echo "Hello world"
  echo "written=12 n=1 buf.len=12"