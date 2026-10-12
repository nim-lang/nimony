# A const holding `low(int64)` used to crash the compile-time evaluator (#2634).
import std/syncio

const samples = [
  low(int64),
  -4611686018427387904'i64,
  -1'i64,
  0'i64,
  4611686018427387903'i64,
  high(int64),
]

for x in samples:
  echo x

const lo = low(int64)
echo lo == samples[0], " ", -(lo + 1) == high(int64)
