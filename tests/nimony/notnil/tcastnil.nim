# `cast` is the escape hatch. A CONVERSION claims its target type honestly and
# may not launder a `nil` into a not-nil one -- that is what makes `default(T)`
# for a not-nil `T` an error -- but a `cast` says "this representation, on my
# head", the standing `addr` has. `io_uring.prepRw` hands C its NULL addr
# exactly this way, so taking it away would leave no way to write one.
proc take(p: pointer) = discard

take(cast[pointer](nil))
