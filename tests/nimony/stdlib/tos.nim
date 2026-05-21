import std/[os, assertions, syncio]

when defined(posix):
  assert quoteShellPosix("ls") == "ls"
  assert execShellCmd("ls") == 0

proc main =
  let tmpFile = getTempDir() / "nimony_getfilesize_test.tmp"
  try:
    writeFile(tmpFile, "abcd")
    assert getFileSize(tmpFile) == 4'i64
  except:
    quit "getFileSize test failed"

main()
echo "ok"
