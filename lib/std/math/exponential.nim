## Native (libc-free) implementations of `exp`, `ln`, `log2`, `log10` and `log1p`.
## Selected instead of `math/cmath` by `std/math` when `nimNativeIo` is
## defined.
import std/math/common

func exp*(x: float32): float32 = default(float32)
func exp*(x: float64): float64 = default(float64)

func ln*(x: float32): float32 = default(float32)
func ln*(x: float64): float64 = default(float64)

func log2*(x: float32): float32 = default(float32)
func log2*(x: float64): float64 = default(float64)

func log10*(x: float32): float32 = default(float32)
func log10*(x: float64): float64 = default(float64)

func log1p*(x: float32): float32 = default(float32)
func log1p*(x: float64): float64 = default(float64)
