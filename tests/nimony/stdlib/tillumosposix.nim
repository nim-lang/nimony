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
  assert opendir(name.toCString) == nil
  assert errno() == ENOENT
  assert readdir(nil) == nil
  assert errno() == EBADF
  assert closedir(nil) == -1
  assert errno() == EBADF
  assert posix_fallocate(-1, Off(0), Off(4096)) == EBADF

  let dir = opendir(cstring("/tmp"))
  assert dir != nil
  var entries = 0
  while true:
    let entry = readdir(dir)
    if entry == nil:
      assert errno() == 0
      break
    assert entry.d_type == DT_UNKNOWN
    inc entries
  assert entries >= 2 # . and ..
  assert closedir(dir) == 0

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
