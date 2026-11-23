# Sem-checked for --cpu:arm (Linux and Android) by setup.nim: std/posix needs
# the 32-bit ARM ABI, whose `struct stat64` is 104 bytes (st_size at 48).
import std/[os, syncio, locks, rawthreads]
import std/posix/posix
import std/crypto/sysrand

when sizeof(Stat) != 104:
  {.error: "Linux/ARM struct stat64 is 104 bytes".}

var lock: Lock
initLock lock
var buf = default(array[16, byte])
echo paramCount(), " ", urandom(buf), " ", getThreadId()
