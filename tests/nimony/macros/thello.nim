# Smoke test for the macro plugin pipeline: a zero-arg macro builds a
# `(call echo "...")` tree at compile time, the plugin emits the NIF, and
# the call site is replaced with the echo invocation.
import std / [syncio, macros]

macro hello(): untyped =
  result = newCall("echo", [newStrLitNode("hello from macro!")])

hello()
