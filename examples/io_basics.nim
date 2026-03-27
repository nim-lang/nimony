# File I/O basics
# ===============
#
# Nim uses `syncio` for synchronous file operations.
# `echo` and `stdout`/`stderr` are always available.

import std/[syncio, assertions]

# --- Writing and reading files ---

let filename = "nimcache/_example_tmp.txt"

# Write a file:
var f: File
if open(f, filename, fmWrite):
  f.writeLine "first line"
  f.writeLine "second line"
  f.writeLine "third line"
  f.close

# Read it back line by line:
var lines: seq[string] = @[]
var line: string = ""
if open(f, filename, fmRead):
  while f.readLine(line):
    lines.add line
  f.close

assert lines.len == 3
assert lines[0] == "first line"
assert lines[1] == "second line"
assert lines[2] == "third line"

# --- tryWriteFile for quick writes ---

assert tryWriteFile(filename, "hello from tryWriteFile")

# --- echo writes to stdout with newline ---

echo "io_basics: OK"
