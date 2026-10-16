#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Cryptographically secure random bytes from the operating system.
##
## Key material must come from here and never from `std/random`, whose
## generator is fast, seedable and therefore predictable.
##
## | Target              | Source                                       |
## | :---                | :---                                         |
## | Windows             | `BCryptGenRandom` (the system-preferred RNG) |
## | Linux               | `getrandom`, `/dev/urandom` without it       |
## | FreeBSD, illumos    | `getrandom`                                  |
## | macOS               | `getentropy`                                 |
## | other POSIX         | `/dev/urandom`                               |
##
## Linux falls back to `/dev/urandom` on a kernel older than 3.17 (or a
## seccomp filter that rejects `getrandom` with `ENOSYS`). The libc-free
## backend issues the `getrandom` syscall itself.

{.feature: "staticContracts".}

when defined(windows):
  {.passL: "-lbcrypt".}   # `dynlib` externs are static imports, see std/ioring

  const BCRYPT_USE_SYSTEM_PREFERRED_RNG = 0x00000002'u32

  proc bcryptGenRandom(hAlgorithm: nil pointer; pbBuffer: pointer; cbBuffer: uint32;
                       dwFlags: uint32): int32 {.
    stdcall, importc: "BCryptGenRandom", dynlib: "bcrypt.dll", sideEffect.}
    ## An NTSTATUS: `0` (STATUS_SUCCESS) or a negative error.

  proc fill(p: ptr UncheckedArray[byte]; size: int): bool =
    result = true
    var done = 0
    while done < size:
      # `cbBuffer` is a ULONG: a request beyond 4 GiB goes in pieces.
      let n = min(size - done, int(high(int32)))
      if bcryptGenRandom(nil, addr p[done], uint32(n),
                         BCRYPT_USE_SYSTEM_PREFERRED_RNG) != 0'i32:
        return false
      done += n

else:
  import std/posix/posix

  const
    useGetrandom = defined(linux) or defined(freebsd) or defined(illumos)
    useGetentropy = defined(osx)

  proc fillFromDevice(p: ptr UncheckedArray[byte]; size: int): bool =
    ## `/dev/urandom`: never blocks once the pool is initialized and never
    ## runs dry. It must be the character device; anything else at that path
    ## (a regular file in a chroot, say) is not a source of randomness.
    result = false
    let fd = cint(pcall(open(cstring"/dev/urandom", O_RDONLY or O_CLOEXEC)))
    if fd < 0: return
    var st = default(Stat)
    if pcall(fstat(fd, st)) == 0 and S_ISCHR(st.st_mode):
      var done = 0
      while done < size:
        let n = pcall(read(fd, addr p[done], size - done))
        if n > 0: done += int(n)
        elif n != -EINTR: break     # 0 (end of data) is as fatal as an error
      result = done == size
    discard close(fd)

  when useGetrandom:
    proc getrandom(buf: pointer; buflen: csize_t; flags: cuint): int {.
      importc: "getrandom", sideEffect.}
      ## Without flags: the urandom pool, blocking only until it has been
      ## initialized once after boot. A request beyond 256 bytes can return
      ## short when interrupted by a signal, hence the loop below.

    proc fill(p: ptr UncheckedArray[byte]; size: int): bool =
      result = true
      var done = 0
      while done < size:
        let n = pcall(getrandom(addr p[done], csize_t(size - done), 0))
        if n > 0: done += int(n)
        elif n == -EINTR: discard
        elif defined(linux) and n == -ENOSYS and done == 0:
          return fillFromDevice(p, size)
        else: return false

  elif useGetentropy:
    proc getentropy(buf: pointer; buflen: csize_t): cint {.
      importc: "getentropy", sideEffect.}
      ## All or nothing, at most 256 bytes per call.

    proc fill(p: ptr UncheckedArray[byte]; size: int): bool =
      result = true
      var done = 0
      while done < size:
        let n = min(size - done, 256)
        if getentropy(addr p[done], csize_t(n)) != 0: return false
        done += n

  else:
    proc fill(p: ptr UncheckedArray[byte]; size: int): bool =
      fillFromDevice(p, size)

proc urandom*(dest: var openArray[byte]): bool =
  ## Fills `dest` from the operating system's CSPRNG. `false` if it could not
  ## be read; a caller must not proceed with a key then. An empty `dest`
  ## succeeds without asking the OS.
  if dest.len == 0: return true
  result = fill(cast[ptr UncheckedArray[byte]](addr dest[0]), dest.len)
