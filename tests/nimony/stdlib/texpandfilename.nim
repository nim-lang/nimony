# `expandFilename` resolves symlinks like `realpath`, also under the default
# libc-free `nimNativeIo` stdlib (which has no `realpath` to call).

import std/[assertions, os, syncio, dirs, strutils]

proc show(failures: var int; root, label, p, expected: string) =
  ## Checks `p` against `expected`, written relative to `root` (the physical path of the base directory) so it does not
  ## depend on where the temp dir is. Only mismatches are printed, so the output is the same on every platform.
  var got: string
  try:
    let r = expandFilename(p)
    if r == root:
      got = "<base>"
    elif r.startsWith(root & "/"):
      got = "<base>" & substr(r, root.len)
    else:
      got = r
  except ErrorCode as e:
    got = "raises " & $e
  if got != expected:
    echo label, ": expected ", expected, " but got ", got
    inc failures

proc main() {.raises.} =
  var failures = 0
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

    show(failures, root, "plain file", base / "a" / "b" / "f.txt", "<base>/a/b/f.txt")
    show(failures, root, "directory", base / "a" / "b", "<base>/a/b")
    show(failures, root, "trailing slash", base & "/a/b/", "<base>/a/b")
    show(failures, root, "dot and dotdot", base & "/a/./b/../b/./f.txt", "<base>/a/b/f.txt")
    show(failures, root, "doubled slashes", base & "//a///b//f.txt", "<base>/a/b/f.txt")
    show(failures, root, "relative symlink", base / "a" / "rel", "<base>/a/b/f.txt")
    show(failures, root, "absolute symlink", base / "abs", "<base>/c/g.txt")
    show(failures, root, "chain of links", base / "chain2", "<base>/a/b/f.txt")
    show(failures, root, "symlinked dir in the middle", base / "dirlink" / "f.txt", "<base>/a/b/f.txt")
    show(failures, root, "dotdot after a symlinked dir", base & "/dirlink/..", "<base>/a")
    show(failures, root, "dotdot inside a link target", base / "a" / "up" / "g.txt", "<base>/c/g.txt")
    show(failures, root, "dotdot past the root", "/../../" & base & "/a", "<base>/a")

    let cwd = dirs.getCurrentDir()
    setCurrentDir(path(base / "a"))
    show(failures, root, "relative path", "b/f.txt", "<base>/a/b/f.txt")
    show(failures, root, "relative through link", "rel", "<base>/a/b/f.txt")
    show(failures, root, "relative dotdot", "../c/g.txt", "<base>/c/g.txt")
    show(failures, root, "dot", ".", "<base>/a")
    setCurrentDir(cwd)

    show(failures, root, "link loop", base / "loop1", "raises BadLink")
    show(failures, root, "dangling link", base / "dangling", "raises NameNotFound")
    show(failures, root, "missing file", base / "missing.txt", "raises NameNotFound")
    show(failures, root, "missing dir", base / "missing" / "f.txt", "raises NameNotFound")
    show(failures, root, "file used as dir", base / "a" / "b" / "f.txt" / "x", "raises NameNotFound")
    show(failures, root, "file with trailing slash", base & "/a/b/f.txt/", "raises NameNotFound")
    show(failures, root, "file then dotdot", base & "/a/b/f.txt/..", "raises NameNotFound")
    show(failures, root, "link to file used as dir", base / "abs" / "x", "raises NameNotFound")
    show(failures, root, "empty", "", "raises NameNotFound")

    removeDir(path(base))
  # Windows: no POSIX symlink semantics to check; print the same line so one
  # `.output` file serves every platform.
  if failures == 0: echo "expandFilename ok"

try:
  main()
except:
  quit "texpandfilename: unexpected error"
