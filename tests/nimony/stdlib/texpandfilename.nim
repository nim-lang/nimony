# `expandFilename` resolves symlinks like `realpath`, also under the default
# libc-free `nimNativeIo` stdlib (which has no `realpath` to call).

import std/[assertions, os, syncio, dirs, strutils]

proc show(root, label, p: string) =
  ## Prints `p` relative to `root` (the physical path of the base directory) so the output does not depend on where the
  ## temp dir is.
  try:
    let r = expandFilename(p)
    if r == root:
      echo label, ": <base>"
    elif r.startsWith(root & "/"):
      echo label, ": <base>", substr(r, root.len)
    else:
      echo label, ": ", r
  except ErrorCode as e:
    echo label, ": raises ", e

proc main() {.raises.} =
  when defined(posix):
    let base = getTempDir() / "nimony_texpandfilename"
    removeDir(path(base))
    createDir(path(base / "a" / "b"))
    createDir(path(base / "c"))
    writeFile(base / "a" / "b" / "f.txt", "x")
    writeFile(base / "c" / "g.txt", "y")
    # Paths with `.`, `..` or a trailing slash are built with `&`: `/` would
    # normalize them away lexically before `expandFilename` sees them.
    # the temp dir itself may sit behind a symlink (macOS `/tmp`)
    let root = expandFilename(base)
    assert root.isAbsolute and not root.endsWith("/")

    createSymlink("b/f.txt", base / "a" / "rel")            # relative
    createSymlink(base / "c" / "g.txt", base / "abs")       # absolute
    createSymlink("rel", base / "a" / "chain1")             # chain1 -> rel -> b/f.txt
    createSymlink("a/chain1", base / "chain2")              # chain2 -> a/chain1
    createSymlink("a/b", base / "dirlink")                  # a symlinked dir
    createSymlink("../c", base / "a" / "up")                # a/up -> ../c
    createSymlink("loop2", base / "loop1")                  # loop1 <-> loop2
    createSymlink("loop1", base / "loop2")
    createSymlink("nowhere", base / "dangling")

    show(root, "plain file", base / "a" / "b" / "f.txt")
    show(root, "directory", base / "a" / "b")
    show(root, "trailing slash", base & "/a/b/")
    show(root, "dot and dotdot", base & "/a/./b/../b/./f.txt")
    show(root, "doubled slashes", base & "//a///b//f.txt")
    show(root, "relative symlink", base / "a" / "rel")
    show(root, "absolute symlink", base / "abs")
    show(root, "chain of links", base / "chain2")
    show(root, "symlinked dir in the middle", base / "dirlink" / "f.txt")
    show(root, "dotdot after a symlinked dir", base & "/dirlink/..")
    show(root, "dotdot inside a link target", base / "a" / "up" / "g.txt")
    show(root, "dotdot past the root", "/../../" & base & "/a")

    let cwd = dirs.getCurrentDir()
    setCurrentDir(path(base / "a"))
    show(root, "relative path", "b/f.txt")
    show(root, "relative through link", "rel")
    show(root, "relative dotdot", "../c/g.txt")
    show(root, "dot", ".")
    setCurrentDir(cwd)

    show(root, "link loop", base / "loop1")
    show(root, "dangling link", base / "dangling")
    show(root, "missing file", base / "missing.txt")
    show(root, "missing dir", base / "missing" / "f.txt")
    show(root, "file used as dir", base / "a" / "b" / "f.txt" / "x")
    show(root, "file with trailing slash", base & "/a/b/f.txt/")
    show(root, "file then dotdot", base & "/a/b/f.txt/..")
    show(root, "link to file used as dir", base / "abs" / "x")
    show(root, "empty", "")

    removeDir(path(base))
  else:
    echo "skipped"

try:
  main()
except:
  quit "texpandfilename: unexpected error"
