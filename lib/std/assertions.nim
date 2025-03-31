import std/syncio

template assert*(cond: bool; msg = "") =
  if not cond:
    echo "[Assertion Failure] ", msg
    quit 1
