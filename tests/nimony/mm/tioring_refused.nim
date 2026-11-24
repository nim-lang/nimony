# nimony-bugs-llms#344: the pool I/O shares refs between threads, so arc and orc
# refuse it at compile time instead of corrupting memory.
import std / ioring
