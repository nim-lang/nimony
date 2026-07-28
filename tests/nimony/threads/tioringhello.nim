import std/ioring
import std/syncio

var ring = initIoRing()
var buf = "Hello world\n"
discard ring.submitWrite(stdout.getFileHandle, buf.toCString, buf.len)
var comps: array[16, IoCompletion]
let n = ring.waitCompletions(comps)
echo "written=", comps[0].result, " n=", n, " buf.len=", buf.len
ring.shutdown()