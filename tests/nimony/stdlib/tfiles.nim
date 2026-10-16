# std/files: copyFile, copyFileWithPermissions, copyFileToDir, moveFile and
# the Path permission accessors. The cross-device (EXDEV) fallback of
# moveFile is not exercised: it needs two filesystems.

import std/[assertions, os, syncio, dirs, symlinks]
import std/files

proc main() {.raises.} =
  when defined(posix):
    let base = path(getTempDir()) / path("nimony_tfiles")
    for f in ["small", "big", "empty", "small2", "big2", "empty2", "over",
              "exe", "exe2", "exe3", "link", "linkFollow", "linkAsIs",
              "linkIgnore", "sub/small", "mvA", "mvB", "mvC", "mvLink",
              "mvLink2"]:
      discard tryRemoveFile(base / path(f))
    removeDir(base / path("sub"))
    removeDir(base)
    createDir(base)

    # content copy, including a file several times larger than the buffer
    let small = base / path("small")
    writeFile($small, "hello\nworld\n")
    copyFile(small, base / path("small2"))
    assert readFile($(base / path("small2"))) == "hello\nworld\n"

    var big = newString(100_000)
    for i in 0 ..< big.len: big[i] = char(i mod 251)
    writeFile($(base / path("big")), big)
    copyFile(base / path("big"), base / path("big2"), bufferSize = 4096)
    assert readFile($(base / path("big2"))) == big

    # empty file
    writeFile($(base / path("empty")), "")
    copyFile(base / path("empty"), base / path("empty2"))
    assert fileExists($(base / path("empty2")))
    assert readFile($(base / path("empty2"))) == ""

    # overwriting an existing, longer dest truncates it
    writeFile($(base / path("over")), "a much longer previous content")
    copyFile(small, base / path("over"))
    assert readFile($(base / path("over"))) == "hello\nworld\n"

    # copyFileToDir
    createDir(base / path("sub"))
    copyFileToDir(small, base / path("sub"))
    assert readFile($(base / path("sub/small"))) == "hello\nworld\n"

    # permissions: copyFile gives a new file default permissions, while
    # copyFileWithPermissions carries the exec bit over
    let exe = base / path("exe")
    writeFile($exe, "#!/bin/sh\n")
    setFilePermissions(exe, {fpUserRead, fpUserWrite, fpUserExec})
    assert getFilePermissions(exe) == {fpUserRead, fpUserWrite, fpUserExec}
    copyFile(exe, base / path("exe2"))
    assert fpUserExec notin getFilePermissions(base / path("exe2"))
    copyFileWithPermissions(exe, base / path("exe3"))
    assert getFilePermissions(base / path("exe3")) ==
      {fpUserRead, fpUserWrite, fpUserExec}
    assert readFile($(base / path("exe3"))) == "#!/bin/sh\n"

    # symlinks, per flag
    let link = base / path("link")
    createSymlink(path("small"), link)
    copyFile(link, base / path("linkFollow"))
    assert not symlinkExists(base / path("linkFollow"))
    assert readFile($(base / path("linkFollow"))) == "hello\nworld\n"
    copyFile(link, base / path("linkAsIs"), cfSymlinkAsIs)
    assert symlinkExists(base / path("linkAsIs"))
    assert $expandSymlink(base / path("linkAsIs")) == "small"
    copyFile(link, base / path("linkIgnore"), cfSymlinkIgnore)
    assert not fileExists($(base / path("linkIgnore")))
    assert not symlinkExists(base / path("linkIgnore"))

    # moveFile: a rename within the directory
    writeFile($(base / path("mvA")), "moved")
    moveFile(base / path("mvA"), base / path("mvB"))
    assert not fileExists($(base / path("mvA")))
    assert readFile($(base / path("mvB"))) == "moved"
    # ... overwriting an existing dest
    writeFile($(base / path("mvC")), "old content")
    moveFile(base / path("mvB"), base / path("mvC"))
    assert not fileExists($(base / path("mvB")))
    assert readFile($(base / path("mvC"))) == "moved"
    # ... a symlink moves the link, not the file it points to
    createSymlink(path("mvC"), base / path("mvLink"))
    moveFile(base / path("mvLink"), base / path("mvLink2"))
    assert not symlinkExists(base / path("mvLink"))
    assert symlinkExists(base / path("mvLink2"))
    assert $expandSymlink(base / path("mvLink2")) == "mvC"
    assert readFile($(base / path("mvC"))) == "moved"
    # ... a missing source, which leaves dest alone
    var mvErr = Success
    try:
      moveFile(base / path("missing"), base / path("mvC"))
    except ErrorCode as e:
      mvErr = e
    assert mvErr == NameNotFound
    assert readFile($(base / path("mvC"))) == "moved"
    # the cleanup below expects every name to exist
    writeFile($(base / path("mvA")), "")
    writeFile($(base / path("mvB")), "")
    createSymlink(path("mvC"), base / path("mvLink"))

    # errors: a missing source, a directory as source
    var err = Success
    try:
      copyFile(base / path("missing"), base / path("x"))
    except ErrorCode as e:
      err = e
    assert err == NameNotFound
    assert not fileExists($(base / path("x")))
    err = Success
    try:
      copyFileWithPermissions(base / path("missing"), base / path("x"))
    except ErrorCode as e:
      err = e
    assert err == NameNotFound
    err = Success
    try:
      copyFile(base / path("sub"), base / path("x"))
    except ErrorCode as e:
      err = e
    assert err != Success
    assert not fileExists($(base / path("x")))
    err = Success
    try:
      discard getFilePermissions(base / path("missing"))
    except ErrorCode as e:
      err = e
    assert err == NameNotFound

    for f in ["small", "big", "empty", "small2", "big2", "empty2", "over",
              "exe", "exe2", "exe3", "link", "linkFollow", "linkAsIs",
              "linkIgnore", "sub/small", "mvA", "mvB", "mvC", "mvLink",
              "mvLink2"]:
      removeFile(base / path(f))
    removeDir(base / path("sub"))
    removeDir(base)
  echo "tfiles: ok"

try:
  main()
except:
  quit "tfiles: unexpected error"
