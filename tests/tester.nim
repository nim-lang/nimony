
import std / [os, strutils]
import "../src/lib" / [nifreader, nifbuilder, nif_linkedtree, treemangler]

const
  ExpectedOutput = """
(stmts
 (comment
 (import
 (import
 (when
 (importExcept
 (from
 (from
 (from
 (const
 (when
 (proc
 (proc
 (when
 (template
 (proc
 (when
 (proc
 (template
 (template
 (template
 (template
 (template
 (template
 (template
 (proc
 (proc
 (include
 (template
 (template
 (proc
 (template
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (template
 (proc
 (proc
 (type
 (proc
 (proc
 (proc
 (proc
 (template
 (proc
 (proc
 (const
 (const
 (const
 (const
 (const
 (const
 (template
 (template
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (include
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (proc
 (iterator
 (const
 (proc
"""

proc outline*(n: NifTree): string =
  result = $n.tok & "\n"
  var it = n.down
  while it != nil:
    result.add " " & $it.tok & "\n"
    it = it.next

proc test*(filename: string) =
  var r = nifreader.open(filename)
  let res = processDirectives(r)
  assert res == Success
  let t = parse(r)
  assert outline(t) == ExpectedOutput
  r.close()

const
  SubsResult = """
(stmts
 (call
  Symbol:echo.1.sys StringLit:Hello World!\0A) (call
  Symbol:echo.1.sys StringLit:different string))"""

proc subsTest(filename: string): string =
  var r = nifreader.open(filename)
  let res = processDirectives(r)
  assert res == Success
  let t = parse(r)
  result = $t
  r.close()

test "tests/data/vm.nif"

let st = subsTest("tests/data/tsubs.nif")
assert st == SubsResult

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

proc testXelim(overwrite: bool) =
  exec "nim c src/xelim/xelim"
  var toRemove: seq[string] = @[]
  for k, f in walkDir("tests/xelim"):
    if f.endsWith(".nif") and not f.endsWith(".xelim.nif") and not f.endsWith(".expected.nif"):
      exec ("src" / "xelim" / "xelim").addFileExt(ExeExt) & " " & f
      let r = f.withExt(".xelim.nif")
      let e = f.withExt(".expected.nif")
      if not os.sameFileContent(r, e):
        if overwrite:
          moveFile(r, e)
        else:
          fatal "files have not the same content: " & e & " " & r
      else:
        toRemove.add r
  for rem in toRemove:
    removeFile rem

let overwrite = os.paramCount() > 0 and os.paramStr(1) == "--overwrite"
testXelim(overwrite)

proc testIndexer(overwrite: bool) =
  exec "nim c src/lib/nifindexes"
  var toRemove: seq[string] = @[]
  let f = "tests/nifc/hello.nif"
  exec ("src" / "lib" / "nifindexes").addFileExt(ExeExt) & " " & f

  let r = f.withExt(".idx.nif")
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

proc hasturTests() =
  exec "nim c -r src/hastur"

hasturTests()
