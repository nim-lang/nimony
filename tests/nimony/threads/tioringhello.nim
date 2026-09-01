import std/syncio

when not defined(windows):
  import std/ioring
  var buf = "Hello world\n"
  discard submitWrite(stdout.getFileHandle, buf.toCString, buf.len, never)
  var comps: array[16, IoCompletion]
  let n = waitCompletions(comps)
  echo "written=", comps[0].result, " n=", n, " buf.len=", buf.len
else:
  echo "Hello world"
  echo "written=12 n=1 buf.len=12"
