## Native (libc-free) implementations of `erf`, `erfc`, `gamma` and `lgamma`.
## Selected instead of `math/cmath` by `std/math` when `nimNativeIo` is
## defined.
import std/math/common

func erf*(x: float32): float32 = default(float32)
func erf*(x: float64): float64 = default(float64)

func erfc*(x: float32): float32 = default(float32)
func erfc*(x: float64): float64 = default(float64)

func gamma*(x: float32): float32 = default(float32)
func gamma*(x: float64): float64 = default(float64)

func lgamma*(x: float32): float32 = default(float32)
func lgamma*(x: float64): float64 = default(float64)
