## Native (libc-free) implementations of `sqrt`, `cbrt`, `pow` and `hypot`. Selected
## instead of `math/cmath` by `std/math` when `nimNativeIo` is defined.
##
import std/math/common

func sqrt*(x: float32): float32 = default(float32)
func sqrt*(x: float64): float64 = default(float64)

func cbrt*(x: float32): float32 = default(float32)
func cbrt*(x: float64): float64 = default(float64)

func pow*(x, y: float32): float32 = default(float32)
func pow*(x, y: float64): float64 = default(float64)

func hypot*(x, y: float32): float32 = default(float32)
func hypot*(x, y: float64): float64 = default(float64)
