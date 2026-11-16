# A constant integer conversion zero-extends an unsigned source
# (planetis-m/nimony-bugs-fuzzers#53).
import std/syncio
var g1 = int64(uint32(-1'i64))
echo g1

# A conversion or cast to `bool` is `x != 0`, not the low byte
# (planetis-m/nimony-bugs-llms#213).
proc f(x: int32): bool = bool(x)
proc g(x: int8): bool = bool(x)
proc h(x: float): bool = bool(x)
proc p(x: pointer): bool = cast[bool](x)
echo bool(256'i32), " ", f(256), " ", ord(bool(2'i32)), " ", ord(g(2)),
  " ", h(0.5), " ", p(cast[pointer](256)), " ", f(0), " ", h(0.0)
