# It supports overwriting tested results using `--overwrite`

import std / [os]
import "../src/lib" / [nifbuilder]

const
  ExpectedNifBuilderResult = """(.nif24)
(.vendor "tester")
(.dialect "niftest")
(stmts 4,5,mymodb.nim
 (call 1,3,mymod.nim foo.3.mymod +3423 +50.4))"""

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

proc testIndexer(overwrite: bool) =
  exec "nim c src/lib/nifindexes"
  var toRemove: seq[string] = @[]
  let f = "tests/nifc/hello.nif"
  exec ("src" / "lib" / "nifindexes").addFileExt(ExeExt) & " " & f

  let r = f.withExt(".s.idx.nif")
  let e = f.withExt(".expected.idx.nif")
  if not os.sameFileContent(r, e):
    if overwrite:
      moveFile(r, e)
    else:
      fatal "files have not the same content: " & e & " " & r
  else:
    toRemove.add r
  for rem in toRemove:
    removeFile rem

testIndexer(overwrite)

proc testNifGram(overwrite: bool) =
  exec "nim c src/nifgram/nifgram"
  var toRemove: seq[string] = @[]
  let f = "src/nifc/nifc_grammar.nif"
  exec ("src" / "nifgram" / "nifgram").addFileExt(ExeExt) & " " & f

  let r = f.withExt(".nim")
  let e = "tests/data/nifc_grammar.nim"
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
  if overwrite:
    exec "nim c -r src/hastur --overwrite"
  else:
    exec "nim c -r src/hastur"

hasturTests(overwrite)
