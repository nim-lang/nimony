# `algorithm.sort` on a degenerate (len < 2) input. Was quarantined as a
# native-backend bug: BOTH the empty and the one-element sort SIGSEGVed on the
# native leg. Both legs agree again.
import std/[syncio, algorithm]
proc cmpInt(x, y: int): int = x - y
var empty: seq[int] = @[]
sort(empty, cmpInt)
echo empty.len                         # 0
var one = @[42]
sort(one, cmpInt)
echo one[0]                            # 42
