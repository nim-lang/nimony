import std/[assertions, syncio]
import std/dynlib

var candidates = newSeq[string]()
libCandidates("libfoo(|.1|.2).so", candidates)
for c in candidates:
  echo c

let lib: LibHandle =
  when defined(windows):
    loadLib("kernel32.dll")
  else:
    loadLib()

assert lib != nil
when defined(windows):
  assert lib.symAddr("GetLastError") != nil
else:
  assert lib.symAddr("malloc") != nil

try:
  discard checkedSymAddr(lib, "nimony_dynlib_missing_symbol")
  echo "missing symbol did not raise"
except ErrorCode as e:
  assert e == IOError
  echo "missing symbol raised IOError"

unloadLib(lib)
assert loadLibPattern("libnimony_definitely_missing(|.1).so") == nil
echo "dynlib ok"
