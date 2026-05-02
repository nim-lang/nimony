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

import std / [memfiles, syncio, monotimes, times]

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

when defined(nimony):
  import std / [os, dirs, paths, times]
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
  proc readBytes(p: string): string =
    try: readFile(p) except: ""
  proc writeBytes(p, c: string) =
    try: writeFile(p, c) except: quit "vfs: write failed: " & p
  proc fileMaybeExists(p: string): bool =
    try: fileExists(p) except: false
  proc openMmapImpl(p: string): MemFile =
    try: memfiles.open(p) except: quit "vfs: open failed: " & p
else:
  import std / [os, times]
  proc unixModTime(path: string): int64 =
    let t = getLastModificationTime(path)
    toUnix(t) * nanosPerSec + int64(t.nanosecond)
  proc unixNow(): int64 =
    let t = getTime()
    toUnix(t) * nanosPerSec + int64(t.nanosecond)
  proc rmPath(path: string) = removeFile(path)
  proc readBytes(p: string): string = readFile(p)
  proc writeBytes(p, c: string) = writeFile(p, c)
  proc fileMaybeExists(p: string): bool = fileExists(p)
  proc openMmapImpl(p: string): MemFile = memfiles.open(p)

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
