## Native (libc-free) implementations of `floor`, `ceil`, `round`, `trunc`, `mod` and
## `copySign`. Selected instead of `math/cmath` by `std/math` when
## `nimNativeIo` is defined.
import std/math/common

func copySign*(x, y: float32): float32 = default(float32)
func copySign*(x, y: float64): float64 = default(float64)

func floor*(x: float32): float32 = default(float32)
func floor*(x: float64): float64 = default(float64)

func ceil*(x: float32): float32 = default(float32)
func ceil*(x: float64): float64 = default(float64)

func round*(x: float32): float32 = default(float32)
func round*(x: float64): float64 = default(float64)

func trunc*(x: float32): float32 = default(float32)
func trunc*(x: float64): float64 = default(float64)

func `mod`*(x, y: float32): float32 = default(float32)
func `mod`*(x, y: float64): float64 = default(float64)
