#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Virtual filesystem abstraction for nimony, hexer, nifc, and nifmake.
##
## Built on the **relay** pattern from
## https://nim-lang.org/blog/relays.rst — every file operation is a
## module-level proc variable initialized to a default that calls
## straight through to the OS. Drivers reassign at startup; adapters
## capture the previous relay and wrap it (logging, sandboxing, in-memory
## cache, build cache, …).
##
## Day-zero behavior is identical to direct syncio/memfiles/os calls.
## The point is the swappable seam, not yet the swapping.

when defined(nimony):
  {.feature: "lenientnils".}

##
## Mmap'd reads return a `VfsBlob` rather than a raw `MemFile`. The blob
## carries an opaque cookie + cleanup proc supplied by the backend; its
## destructor calls cleanup so e.g. an LMDB-backed driver can close the
## read-transaction that owns its mmap'd pointer at exactly the right
## time. The default disk backend wraps a `MemFile`; an LMDB backend
## wraps a `MDB_txn`; a sandbox adapter would wrap whatever its source
## blob is and refuse to provide one for paths outside the allow-list.

import std / [memfiles, syncio, times]

# --- profiling --------------------------------------------------------
#
# Compile with `-d:vfsProfile` to record per-op counts and elapsed time.
# Each process emits a one-line summary at exit; the build driver
# invokes many short-lived processes, so `awk` over the lines aggregates
# totals across the whole build.

when defined(vfsProfile):
  type
    VfsCounter* = object
      count*: int
      ns*: int64
      bytes*: int64

  var
    statOpenMmap*: VfsCounter
    statRead*:     VfsCounter
    statWrite*:    VfsCounter
    statExists*:   VfsCounter
    statMtime*:    VfsCounter
    statRemove*:   VfsCounter

  import std/strutils

  proc dumpVfsProfile*(label: string) =
    template emit(name: string; c: VfsCounter) =
      stderr.writeLine "[vfs] " & label & " " & name &
        " count=" & $c.count &
        " ms=" & formatFloat(float(c.ns) / 1_000_000.0, ffDecimal, 3) &
        " bytes=" & $c.bytes
    emit "openMmap", statOpenMmap
    emit "read",     statRead
    emit "write",    statWrite
    emit "exists",   statExists
    emit "mtime",    statMtime
    emit "remove",   statRemove
else:
  template dumpVfsProfile*(label: string) = discard

const nanosPerSec: int64 = 1_000_000_000'i64

type
  FileWriteMode* = enum
    AlwaysWrite,
    OnlyIfChanged

# --- atomic replacement ---------------------------------------------------
#
# Never truncate a .nif or .bif that a reader may have mmap'd.

import std / atomics

when defined(windows):
  proc osProcessId(): int32 {.importc: "GetCurrentProcessId", stdcall,
                              dynlib: "kernel32".}
else:
  proc osProcessId(): int32 {.importc: "getpid".}

when defined(nimony):
  var atomicWriteCounter: int = 0
  proc nextTempSeq(): int = atomicFetchAdd(atomicWriteCounter, 1)
else:
  var atomicWriteCounter: Atomic[int]
  proc nextTempSeq(): int = atomicWriteCounter.fetchAdd(1)

proc atomicTempPath*(target: string): string =
  ## A sibling temp path for an atomic replacement of `target`
  result = target & ".tmp." & $int(osProcessId()) & "." & $nextTempSeq()

when defined(nimony):
  import std / [os, dirs, paths]
  import std / private / oscommons
  # Nimony's stdlib procs are `.raises`. Wrap them so vfs.nim stays
  # non-raising (the rest of the compiler is wired that way).
  proc unixModTime(p: string): int64 =
    try: getLastModificationTime(p) except: 0'i64
  proc unixNow(): int64 =
    try:
      let t = getTime()
      toUnix(t) * nanosPerSec + int64(t.nanosecond)
    except: 0'i64
  proc rmPath(p: string) =
    try: removeFile(path(p)) except: discard
  proc moveIntoImpl(src, dst: string): bool =
    try: tryMoveFSObject(src, dst, false) except: false
  proc claimDirImpl(d: string): bool =
    try: tryCreateFinalDir(path(d)) == Success except: false
  proc releaseDirImpl(d: string) =
    try: discard tryRemoveFinalDir(path(d)) except: discard
  when defined(windows):
    import std / windows / winlean
    proc sleepImpl(ms: int) = winlean.sleep(DWORD(ms))
  else:
    import std / posix / posix
    proc sleepImpl(ms: int) =
      var req: Timespec = default(Timespec)
      var rem: Timespec = default(Timespec)
      req.tv_sec = posix.Time(ms div 1000)
      req.tv_nsec = clong((ms mod 1000) * 1_000_000)
      discard nanosleep(req, rem)
  proc readBytes(p: string): string =
    try: readFile(p) except: ""

  proc writeBytes(p, c: string) =
    let tmp = atomicTempPath(p)
    var ok = false
    try:
      writeFile(tmp, c)
      ok = moveIntoImpl(tmp, p)
    except:
      ok = false
    if not ok:
      try: removeFile(path(tmp)) except: discard   # no `.tmp.NNN` litter
      quit "vfs: write failed: " & p

  proc fileMaybeExists(p: string): bool =
    try: fileExists(p) except: false
  proc openMmapImpl(p: string): MemFile =
    try: memfiles.open(p) except: quit "vfs: open failed: " & p
else:
  import std / [os]
  proc unixModTime(path: string): int64 =
    let t = getLastModificationTime(path)
    toUnix(t) * nanosPerSec + int64(t.nanosecond)
  proc unixNow(): int64 =
    let t = getTime()
    toUnix(t) * nanosPerSec + int64(t.nanosecond)
  proc rmPath(path: string) = removeFile(path)
  proc moveIntoImpl(src, dst: string): bool =
    try: moveFile(src, dst); true except CatchableError: false
  proc claimDirImpl(d: string): bool =
    try: not existsOrCreateDir(d) except CatchableError: false
  proc releaseDirImpl(d: string) =
    try: removeDir(d) except CatchableError: discard
  proc sleepImpl(ms: int) = os.sleep(ms)
  proc readBytes(p: string): string = readFile(p)
  proc writeBytes(p, c: string) =
    let tmp = atomicTempPath(p)
    try:
      writeFile(tmp, c)
      if not moveIntoImpl(tmp, p): raise newException(IOError, "move failed: " & p)
    except CatchableError:
      try: removeFile(tmp) except CatchableError: discard   # no `.tmp.NNN` litter
      raise
  proc fileMaybeExists(p: string): bool = fileExists(p)
  proc openMmapImpl(p: string): MemFile = memfiles.open(p)

proc vfsMoveInto*(src, dst: string): bool =
  ## Move `src` onto `dst` as a single filesystem operation, replacing whatever
  ## was there. The point is that `dst` is never opened for writing: a reader
  ## that mmap'd it, or a process currently EXECUTING it, keeps the old inode
  ## and is undisturbed, while everyone who opens the path afterwards sees the
  ## complete new file. There is no window in which `dst` is half-written.
  ##
  ## This is what makes a build artefact safe to publish from several processes
  ## at once. Writing one in place is not: a concurrent `execve` of a partially
  ## written executable fails with ETXTBSY ("Text file busy"), and so does
  ## writing one that somebody else is executing.
  moveIntoImpl(src, dst)

proc vfsTryClaimDir*(dir: string): bool =
  ## Create `dir`, returning true ONLY in the process that created it.
  ##
  ## `mkdir` is the primitive here because it is the one filesystem call that
  ## both creates and reports "somebody beat me to it", atomically, with no
  ## shared runtime between the processes. That makes a directory the cheapest
  ## inter-process lock available to a compiler that fans out into independent
  ## tool processes. Pair it with `vfsReleaseDir`.
  claimDirImpl(dir)

proc vfsSleepMs*(ms: int) =
  ## Portable millisecond sleep, for backing off while another process holds a
  ## lock taken with `vfsTryClaimDir`. Nimony's stdlib has no `os.sleep`, so
  ## this is `nanosleep` there and `Sleep` on Windows — the same idiom
  ## `std/osproc` already uses for its own timeout waits.
  sleepImpl(ms)

proc vfsReleaseDir*(dir: string) =
  ## Drop a lock taken with `vfsTryClaimDir`. Best effort: failing to release
  ## must never fail a build, and a lock that outlives its owner is expected to
  ## be broken by a timeout on the waiting side rather than by cleanup here.
  releaseDirImpl(dir)

# --- relays ---------------------------------------------------------------
#
# Mtimes are exposed as `int64` nanoseconds since the Unix epoch. Whole-second
# resolution caused spurious rebuilds when an output landed in the same wall
# second as one of its inputs (input mtime tied with output mtime → `>=`
# triggered rebuild). nifmake's `<` / `>=` comparisons keep their meaning.

# --- VfsBlob: backend-owned, mmap-friendly read handle -------------------

type
  VfsBlob* = object
    ## Opaque mmap'd read handle. `data` points at `size` bytes of
    ## contiguous read-only content. The disk-backed default fills `mf`
    ## inline and uses `closeMemFileBlob` as cleanup; alternative
    ## backends (LMDB, in-memory cache, …) leave `mf` zero-initialised
    ## and use `cookie` for whatever they need to release. The caller
    ## drops the blob via `closeBlob`, which calls cleanup. Explicit
    ## close — same lifecycle shape as the `MemFile` it replaces.
    data*: pointer
    size*: int
    mf: MemFile
    cookie*: pointer
    cleanup: proc (b: var VfsBlob) {.nimcall.}

proc initBlob*(data: pointer; size: int;
               cookie: pointer = nil;
               cleanup: proc (b: var VfsBlob) {.nimcall.} = nil): VfsBlob =
  ## Constructor for non-MemFile backends.
  VfsBlob(data: data, size: size, cookie: cookie, cleanup: cleanup)

proc closeBlob*(b: var VfsBlob) =
  ## Release the backend resource (mmap unmap, LMDB read txn close, …).
  ## Safe to call multiple times — the second call is a no-op.
  if b.cleanup != nil:
    b.cleanup(b)
  b.data = nil
  b.size = 0
  b.mf = default(MemFile)
  b.cookie = nil
  b.cleanup = nil

# Default disk backend: wrap a MemFile in a VfsBlob whose cleanup runs
# `memfiles.close`. MemFile is plain data (no destructor) so we just
# copy its fields into the blob — no heap alloc per open.

proc closeMemFileBlob(b: var VfsBlob) {.nimcall.} =
  try: memfiles.close(b.mf) except: discard

proc fromMemFile*(mf: sink MemFile): VfsBlob =
  ## Wrap an already-opened MemFile in a blob whose cleanup closes it.
  result = VfsBlob(data: mf.mem, size: mf.size, mf: mf,
                   cookie: nil, cleanup: closeMemFileBlob)

var openMmapRelay*: proc (path: string): VfsBlob {.nimcall.} =
  proc (path: string): VfsBlob = fromMemFile(openMmapImpl(path))

var readBytesRelay*: proc (path: string): string {.nimcall.} =
  proc (path: string): string = readBytes(path)

var writeBytesRelay*: proc (path, content: string) {.nimcall.} =
  proc (path, content: string) = writeBytes(path, content)

var existsRelay*: proc (path: string): bool {.nimcall.} =
  proc (path: string): bool = fileMaybeExists(path)

var mtimeRelay*: proc (path: string): int64 {.nimcall.} =
  proc (path: string): int64 = unixModTime(path)

var nowRelay*: proc (): int64 {.nimcall.} =
  proc (): int64 = unixNow()

var removeRelay*: proc (path: string) {.nimcall.} =
  proc (path: string) = rmPath(path)

# --- portable wrappers ----------------------------------------------------

when defined(vfsProfile):
  proc vfsOpenMmap*(path: string): VfsBlob =
    let t0 = getMonoTime()
    result = openMmapRelay(path)
    inc statOpenMmap.count
    statOpenMmap.bytes += result.size
    statOpenMmap.ns += inNanoseconds(getMonoTime() - t0)

  proc vfsRead*(path: string): string =
    let t0 = getMonoTime()
    result = readBytesRelay(path)
    inc statRead.count
    statRead.bytes += result.len
    statRead.ns += inNanoseconds(getMonoTime() - t0)

  proc vfsWrite*(path, content: string) =
    let t0 = getMonoTime()
    writeBytesRelay(path, content)
    inc statWrite.count
    statWrite.bytes += content.len
    statWrite.ns += inNanoseconds(getMonoTime() - t0)

  proc vfsExists*(path: string): bool =
    let t0 = getMonoTime()
    result = existsRelay(path)
    inc statExists.count
    statExists.ns += inNanoseconds(getMonoTime() - t0)

  proc vfsMtime*(path: string): int64 =
    let t0 = getMonoTime()
    result = mtimeRelay(path)
    inc statMtime.count
    statMtime.ns += inNanoseconds(getMonoTime() - t0)

  proc vfsNow*(): int64 {.inline.} = nowRelay()

  proc vfsRemove*(path: string) =
    let t0 = getMonoTime()
    removeRelay(path)
    inc statRemove.count
    statRemove.ns += inNanoseconds(getMonoTime() - t0)
else:
  proc vfsOpenMmap*(path: string): VfsBlob {.inline.} = openMmapRelay(path)
  proc vfsRead*(path: string): string {.inline.} = readBytesRelay(path)
  proc vfsWrite*(path, content: string) {.inline.} = writeBytesRelay(path, content)
  proc vfsExists*(path: string): bool {.inline.} = existsRelay(path)
  proc vfsMtime*(path: string): int64 {.inline.} = mtimeRelay(path)
  proc vfsNow*(): int64 {.inline.} = nowRelay()
  proc vfsRemove*(path: string) {.inline.} = removeRelay(path)
