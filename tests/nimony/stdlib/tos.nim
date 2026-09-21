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
  try:
    writeFile(tmpFile, [byte 0x7F, 0x45, 0x4C, 0x46, 0])   # the `openArray[byte]` overload
    assert getFileSize(tmpFile) == 5'i64
    assert readFile(tmpFile)[1] == 'E'
    setFilePermissions(tmpFile, {fpUserRead, fpUserWrite, fpUserExec})
  except:
    quit "writeFile(bytes)/setFilePermissions test failed"
  var raised = false
  try:
    setFilePermissions(tmpFile & ".missing", {fpUserRead})
  except:
    raised = true
  assert raised

main()
echo "ok"
