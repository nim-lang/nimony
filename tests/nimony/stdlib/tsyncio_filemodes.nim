import std/[syncio, assertions]

# Keep this test independent of std/os and its platform-specific Stat ABI.
proc removeFile(path: cstring = cstring"tsyncio_filemodes.tmp"): cint {.
  importc: "remove", header: "<stdio.h>".}

const filename = "tsyncio_filemodes.tmp"
discard removeFile()

proc checkContents(expected: string) =
  var f: File
  assert open(f, filename, fmRead)
  var buf = default(array[32, char])
  let n = readBuffer(f, addr buf[0], buf.len)
  assert n == expected.len
  for i in 0 ..< n:
    assert buf[i] == expected[i]
  close(f)

var f: File
# Neither read nor read/write-existing may create a missing file.
assert not open(f, filename, fmRead)
assert not open(f, filename, fmReadWriteExisting)

# Create, then truncate: stale contents must not survive the second open.
assert open(f, filename, fmWrite)
f.write "abcdef"
close(f)
checkContents("abcdef")
assert open(f, filename, fmWrite)
f.write "xy"
close(f)
checkContents("xy")

# Append must preserve existing bytes, and also create a missing file.
assert open(f, filename, fmAppend)
f.write "z"
close(f)
checkContents("xyz")
assert removeFile() == 0
assert open(f, filename, fmAppend)
f.write "abc"
close(f)
checkContents("abc")

# Existing read/write preserves length and writes at the initial position.
assert open(f, filename, fmReadWriteExisting)
f.write "X"
close(f)
checkContents("Xbc")

# Read/write truncates an existing file and creates a missing one.
assert open(f, filename, fmReadWrite)
f.write "q"
close(f)
checkContents("q")
assert removeFile() == 0
assert open(f, filename, fmReadWrite)
f.write "r"
close(f)
checkContents("r")
assert removeFile() == 0
