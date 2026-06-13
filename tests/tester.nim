# It supports overwriting tested results using `--overwrite`

import std / [os]
import "../src/lib" / [nifbuilder]

const
  ExpectedNifBuilderResult = """(.nif27)
(.vendor "tester")
(.dialect "niftest")
(stmts@4,5,mymodb.nim
 (call@1,3,mymod.nim foo.3.mymod 3423 50.4))"""

proc buildSomething(b: sink Builder): string =
  b.addHeader "tester", "niftest"
  b.withTree "stmts":
    b.addLineInfo 4, 5, "mymodb.nim"
    b.withTree "call":
      b.addLineInfo 1, 3, "mymod.nim"
      b.addSymbol "foo.3.mymod"
      b.addIntLit 3423
      b.addFloatLit 50.4

  assert(not b.attachedToFile)
  result = b.extract()

proc testNifBuilder() =
  var b = open(10)
  assert buildSomething(b) == ExpectedNifBuilderResult

testNifBuilder()

proc fatal(msg: string) = quit "FAILURE " & msg

proc exec(cmd: string) =
  if execShellCmd(cmd) != 0: fatal cmd

proc withExt(f, ext: string): string =
  result = f.changeFileExt(ext)
  if not fileExists(result):
    fatal "cannot find output file " & result

let overwrite = os.paramCount() > 0 and os.paramStr(1) == "--overwrite"

proc testNifGram(overwrite: bool) =
  exec "nim c src/nifgram/nifgram"
  var toRemove: seq[string] = @[]
  let f = "src/nifgram/examples/test_grammar.nif"
  exec ("src" / "nifgram" / "nifgram").addFileExt(ExeExt) & " " & f

  let r = f.withExt(".nim")
  let e = "tests/data/test_grammar.nim"
  if not os.sameFileContent(r, e):
    if overwrite:
      moveFile(r, e)
    else:
      fatal "files have not the same content: " & e & " " & r
  else:
    toRemove.add r
  for rem in toRemove:
    removeFile rem

testNifGram(overwrite)

proc hasturTests(overwrite: bool) =
  # CI sets NIMONY_CC=clang on Windows so tests exercise the clang-on-MinGW
  # path (faster cc step + native PE TLS). Locally the env var is unset
  # and the gcc default kicks in. On Windows + clang we also force LLD as
  # the linker: clang emits native PE TLS by default but ld.bfd lays out
  # `.tls$` incorrectly, producing binaries that segfault on first TLS
  # access; LLD's layout matches what the loader expects.
  var args = "--jobs:auto"
  let cc = os.getEnv("NIMONY_CC")
  if cc.len > 0:
    args.add " --forward:--cc:" & cc
    when defined(windows):
      if cc == "clang":
        args.add " --forward:--passL:-fuse-ld=lld"
  if overwrite:
    args.add " --overwrite"
  exec "nim c -r src/hastur " & args & " all"

hasturTests(overwrite)
