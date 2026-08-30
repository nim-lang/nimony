## Custom runner: a cache write must never truncate a file a reader has MMAP'D.
##
## `nifreader.open` and `bif.load` both mmap the cache entry they read, and
## `load` deliberately BORROWS the mapped token block for the process lifetime
## rather than copying it. Any writer that replaces such a file with a
## truncating `fmWrite` open invalidates those pages immediately — the reader
## takes SIGBUS past the new EOF, or reads torn bytes in the window before the
## new content lands.
##
## No race is needed to show it: truncation invalidates the mapping at once, so
## one process suffices — store, mmap, store again, then touch the mapping.
## That determinism is why this is a test and not a stress rig; against a
## truncating writer both cases below die with SIGBUS every time.
##
## Two surfaces, because there are two writers: `bif.store` (binary `.bif`) and
## `vfs.vfsWrite` (the relay behind every `.nif` / `.idx.nif` write — nifpools,
## nifindexes, nifbuilder, nifmake).

import std / [os, strutils]
import "../../src/lib/vfs"
import "../../src/lib/bif"
import "../../src/lib/nifcore"

var failures = 0

proc fail(msg: string) =
  echo "  FAIL: ", msg
  inc failures

proc ok(msg: string) =
  echo "  ok: ", msg

proc tempsLeft(target: string): seq[string] =
  ## `.tmp.<pid>.<n>` siblings of `target`. The temp must be gone on the
  ## success path AND the failure path; a leftover would also be picked up by
  ## directory scans looking for outputs.
  result = @[]
  let dir = target.parentDir
  let prefix = target.extractFilename & ".tmp."
  for kind, p in walkDir(if dir.len > 0: dir else: "."):
    if kind == pcFile and p.extractFilename.startsWith(prefix):
      result.add p

# ── surface 1: bif.store, whose mapping is borrowed for the process lifetime ──

proc buildBuf(n: int): TokenBuf =
  result = createTokenBuf(16)
  let tStmts = result.tags.registerTag("stmts")
  let tCall = result.tags.registerTag("call")
  let f = result.pool.filenames.getOrIncl("some/where.nim")
  result.buildTree tStmts:
    result.appendLineInfo f, 1'i32, 0'i32
    for i in 0 ..< n:
      result.buildTree tCall:
        result.addSymUse "some.long.symbol.name.0"
        result.addIntLit int64(i)
        result.addStrLit "a longer interned string"

proc bifCase() =
  echo "bif.store replaces a mapped .bif without truncating it"
  let path = getTempDir() / "nifcache_bif_atomic.bif"
  removeFile path

  var big = buildBuf(50_000)
  store(big, path)
  let bigSize = getFileSize(path)

  var m = load(path)                    # mmap + borrow the token block
  let n = m.buf.len
  if n == 0:
    fail "loaded an empty token buffer"
    return

  var small = buildBuf(10)              # a second writer, a SMALLER file
  store(small, path)
  if getFileSize(path) >= bigSize:
    fail "the second store did not shrink the file; the test proves nothing"

  # Touch every token of the mapping taken before the rewrite. Under a
  # truncating writer this is a SIGBUS, not a wrong answer.
  var acc = 0'u32
  for i in 0 ..< n: acc = acc xor uint32(m.buf[i])
  ok "the borrowed token block survived a smaller rewrite (" & $n & " tokens)"

  let leftovers = tempsLeft(path)
  if leftovers.len > 0: fail "temp files left behind: " & $leftovers
  else: ok "no .tmp.* residue"
  removeFile path

# ── surface 2: vfsWrite, the relay behind every .nif write ───────────────────

proc vfsCase() =
  echo "vfsWrite replaces a mapped .nif without truncating it"
  let path = getTempDir() / "nifcache_vfs_atomic.nif"
  removeFile path

  var big = newStringOfCap(200_000)
  for i in 0 ..< 200_000: big.add 'x'
  vfsWrite(path, big)

  let blob = vfsOpenMmap(path)
  let size = blob.size
  if size != 200_000:
    fail "mmap reported " & $size & " bytes, expected 200000"
    return

  vfsWrite(path, "short\n")
  if getFileSize(path) >= size:
    fail "the rewrite did not shrink the file; the test proves nothing"

  var sum = 0
  let data = cast[ptr UncheckedArray[char]](blob.data)
  for i in 0 ..< size: sum = sum + int(data[i])
  if sum != 200_000 * int('x'):
    fail "the mapping's bytes changed under us (sum " & $sum & ")"
  else:
    ok "the held mapping still reads its original bytes (" & $size & " bytes)"

  let leftovers = tempsLeft(path)
  if leftovers.len > 0: fail "temp files left behind: " & $leftovers
  else: ok "no .tmp.* residue"
  removeFile path

bifCase()
vfsCase()

if failures > 0:
  echo "nifcache: ", failures, " failure(s)"
  quit 1
echo "nifcache: all checks passed"
