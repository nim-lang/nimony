# Symlink API reachable through `std/os` (string) and `std/symlinks` (Path).

import std/[assertions, os, syncio, dirs]
import std/symlinks

proc main() {.raises.} =
  when defined(posix):
    let base = getTempDir() / "nimony_tsymlinks"
    removeDir(path(base))
    createDir(path(base))
    let target = base / "target.txt"
    writeFile(target, "hello")

    # string API via std/os
    let link1 = base / "link1"
    assert not symlinkExists(link1)
    createSymlink(target, link1)
    assert symlinkExists(link1)
    assert expandSymlink(link1) == target
    assert readFile(link1) == "hello"
    assert not symlinkExists(target)

    # Path API via std/symlinks
    let link2 = path(base) / path("link2")
    assert not symlinkExists(link2)
    createSymlink(path("target.txt"), link2)
    assert symlinkExists(link2)
    assert $expandSymlink(link2) == "target.txt"
    assert readFile($link2) == "hello"

    # hard links
    let hard1 = base / "hard1"
    createHardlink(target, hard1)
    assert not symlinkExists(hard1)
    assert readFile(hard1) == "hello"
    let hard2 = path(base) / path("hard2")
    createHardlink(path(target), hard2)
    assert readFile($hard2) == "hello"

    # failure is reported as an error, not silently ignored
    var failed = false
    try:
      createSymlink(target, link1)
    except:
      failed = true
    assert failed
    failed = false
    try:
      discard expandSymlink(target)
    except:
      failed = true
    assert failed

    for f in ["link1", "link2", "hard1", "hard2", "target.txt"]:
      removeFile(path(base / f))
    removeDir(path(base))
  elif defined(windows):
    let base = getTempDir() / "nimony_tsymlinks"
    removeDir(path(base))
    createDir(path(base))
    let target = base / "target.txt"
    writeFile(target, "hello")

    # Creating symlinks needs admin rights or developer mode; only check
    # `expandSymlink` when the link could be made.
    let link1 = base / "link1"
    var linked = true
    try:
      createSymlink(target, link1)
    except:
      linked = false
    if linked:
      assert symlinkExists(link1)
      assert expandSymlink(link1) == target
      let link2 = path(base) / path("link2")
      createSymlink(path("target.txt"), link2)
      assert $expandSymlink(link2) == "target.txt"
      removeFile(path(link1))
      removeFile(link2)

    # a regular file is not a reparse point
    var failed = false
    try:
      discard expandSymlink(target)
    except:
      failed = true
    assert failed

    removeFile(path(target))
    removeDir(path(base))
  echo "tsymlinks: ok"

try:
  main()
except:
  quit "tsymlinks: unexpected error"
