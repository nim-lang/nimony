when defined(illumos):
  import std/[assertions, rlocks]
  import std/posix/posix

  proc mkstemp(path: cstring): cint {.importc: "mkstemp".}
  var name = "/tmp/nimony-posix-XXXXXX"
  let fd = mkstemp(name.toCString)
  assert fd >= 0
  var st = default(Stat)
  assert ftruncate(fd, Off(12345)) == 0
  assert fstat(fd, st) == 0
  assert st.st_size == Off(12345)
  assert S_ISREG(st.st_mode)
  assert stat(name.toCString, st) == 0
  assert st.st_size == Off(12345)
  assert close(fd) == 0
  assert unlink(name.toCString) == 0
  assert open(name.toCString, O_RDONLY) == -1
  assert errno() == ENOENT
  assert pcall(open(name.toCString, O_RDONLY)) == -clong(ENOENT)
  var missing = default(DirStream)
  assert openDirStream(missing, name.toCString) == -clong(ENOENT)
  var ename: cstring = ""
  var etype = DT_UNKNOWN
  assert nextDirEntry(missing, ename, etype) == -clong(EBADF)
  assert closeDirStream(missing) == 0 # never opened: nothing to close
  assert posix_fallocate(-1, Off(0), Off(4096)) == EBADF

  var dir = default(DirStream)
  assert openDirStream(dir, cstring("/tmp")) == 0
  var entries = 0
  while true:
    let r = nextDirEntry(dir, ename, etype)
    assert r >= 0
    if r == 0: break
    assert etype == DT_UNKNOWN # illumos records carry no d_type
    inc entries
  assert entries >= 2 # . and ..
  assert closeDirStream(dir) == 0

  let child = fork()
  assert child >= 0
  if child == 0: exitnow(7)
  var status = cint(0)
  assert waitpid(child, status, 0) == child
  assert WIFEXITED(status)
  assert WEXITSTATUS(status) == 7
  assert not WIFSIGNALED(status)
  assert not WIFCONTINUED(status)

  var lock = default(RLock)
  initRLock(lock)
  acquire(lock)
  assert tryAcquire(lock) # illumos PTHREAD_MUTEX_RECURSIVE is 4, not 1
  release(lock)
  release(lock)
  deinitRLock(lock)

import std/syncio
echo "illumos posix ok"
