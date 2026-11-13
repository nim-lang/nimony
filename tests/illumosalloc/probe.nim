import std/assertions
import "../nimony/sysbasics/talloclifecycle"

assert defined(illumos)
assert defined(nimNativeAlloc)
assert not defined(nimNoLibc)
