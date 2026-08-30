# `float32` conversion + arithmetic, which pulls in `std/system/formatfloat`'s
# `toDecimal32`. Was quarantined as a native-backend bug: the native leg printed
# 0.0 and -1.5881868e-23, and later failed to compile at all once the earlier
# float bugs were fixed — `4 * c - 2 + uint32(lowerBoundaryIsCloser)` reaches
# nifasm as an `add`/`mov` whose SOURCE operand is a `bool`, which arkham threads
# up as the local's own home because a bool needs no widening instruction.
# nifasm now admits a `bool` source there (nativenif `tests/arkham/conv_bool_widen`).
import std/syncio
echo float32(1.5)
echo $(0.1'f32 + 0.2'f32)
