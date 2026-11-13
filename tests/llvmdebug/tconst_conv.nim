# A constant integer conversion zero-extends an unsigned source
# (planetis-m/nimony-bugs-fuzzers#53).
import std/syncio
var g1 = int64(uint32(-1'i64))
echo g1
