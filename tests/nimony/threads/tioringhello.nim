import std/syncio

when not defined(windows):
  import std/ioring
  var buf = "Hello world\n"
  # The ring writes straight to fd 1, around `echo`'s buffering. Anything
  # already echoed is still sitting in that buffer and would be overtaken —
  # which is exactly what made this test's group print out of order when it
  # runs joined with neighbours that echo before it.
  stdout.flushFile()
  discard submitWrite(stdout.getFileHandle, buf.toCString, buf.len, never)
  var comps: array[16, IoCompletion]
  let n = waitCompletions(comps)
  echo "written=", comps[0].result, " n=", n, " buf.len=", buf.len
else:
  echo "Hello world"
  echo "written=12 n=1 buf.len=12"
