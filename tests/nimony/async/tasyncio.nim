## `std/asyncio`'s file surface, exercised end to end: one `.passive` chain
## writes a configuration with `writeFile`, reads it back line by line with
## `open` + `readLine`, seeks back and re-reads the first line, checks the
## `readAll`-style `readFile` equals what was written, and confirms that
## opening a file that does not exist raises. It cleans up its own temp file.
##
## Everything runs in one chain, so it is the only writer and reader of the
## file; its name is unique to this test, so even two parallel hastur runs of
## this group trivially truncate-and-re-write the same constant bytes and
## neither can observe a difference. Only the content is printed, never the
## path or the descriptor, so the golden is deterministic.
import std/[asyncio]
import std/[syncio, threadpool, atomics, assertions, dirs, os]

const ConfigContents = "server = 127.0.0.1\nport = 8080\n# a comment line\ntimeout_ms = 5000"

var configPath = ""
var done: int
var log = ""

proc readConfig() {.passive.} =
  try:
    asyncio.writeFile(configPath, ConfigContents)
    var f = asyncio.open(configPath, fmRead, dl = afterMs(5000))
    let l1 = f.readLine(afterMs(5000))
    let l2 = f.readLine(afterMs(5000))
    let l3 = f.readLine(afterMs(5000))
    let l4 = f.readLine(afterMs(5000))
    let atEof = f.readLine(afterMs(5000))
    f.setFilePos(0)
    let again = f.readLine(afterMs(5000))
    asyncio.close(f)
    log.add "l1=" & l1 & "\n"
    log.add "l2=" & l2 & "\n"
    log.add "l3=" & l3 & "\n"
    log.add "l4=" & l4 & "\n"
    log.add "readLine at eof empty=" & $(atEof.len == 0) & "\n"
    log.add "re-read after setFilePos(0)=" & again & "\n"
    var f2 = asyncio.open(configPath, fmRead, dl = afterMs(5000))
    let whole = readAll(f2, afterMs(5000))
    asyncio.close(f2)
    log.add "readFile round-trip matches=" & $(whole == ConfigContents) & "\n"
    try:
      discard asyncio.open(configPath & ".does_not_exist", dl = afterMs(5000))
      log.add "opening a missing file did not raise\n"
    except ErrorCode:
      log.add "opening a missing file raised\n"
  except ErrorCode as e:
    log.add "error: " & $e & "\n"
  try:
    removeFile(path(configPath))
  except ErrorCode:
    discard
  atomicStore(done, 1, moRelease)

proc awaitFlag(flag: var int) =
  let start = monoNow()
  while atomicLoad(flag, moAcquire) == 0:
    if millisUntil(monoNow(), start) > 30_000: quit "timed out at: " & log
  assert atomicLoad(flag, moAcquire) == 1

configPath = getTempDir() & "/nimony_asyncio_config.cfg"
echo "starting"
submit(delay(readConfig()), 0)
awaitFlag(done)
assert not fileExists(configPath)
stdout.write log
stdout.flushFile()