# std/dirs removeDir: recursive like Nim's. Symlinks inside the tree are
# removed as links and never followed, so an outside target survives.
import std/[assertions, dirs, os, syncio]
import std/symlinks

proc raisesFor(dir: Path): ErrorCode =
  result = Success
  try:
    removeDir(dir, checkDir = true)
  except ErrorCode as e:
    result = e

proc main() {.raises.} =
  let base = path(getTempDir()) / path("nimony_tremovedir")
  let tree = base / path("tree")
  let outside = base / path("outside")
  removeDir(base)

  # a nested tree with files at several levels
  createDir(tree / path("a") / path("b") / path("c"))
  createDir(tree / path("empty"))
  writeFile($(tree / path("top.txt")), "top")
  writeFile($(tree / path("a") / path("mid.txt")), "mid")
  writeFile($(tree / path("a") / path("b") / path("c") / path("deep.txt")), "deep")

  createDir(outside / path("inner"))
  writeFile($(outside / path("keep.txt")), "keep")
  writeFile($(outside / path("inner") / path("keep2.txt")), "keep2")
  when defined(posix):
    # links to an outside directory and to an outside file, and a dangling one
    createSymlink(outside, tree / path("a") / path("dirlink"))
    createSymlink(outside / path("keep.txt"), tree / path("filelink"))
    createSymlink(base / path("nowhere"), tree / path("dangling"))

  removeDir(tree)
  echo dirExists($tree)
  # the link targets and their contents are untouched
  echo readFile($(outside / path("keep.txt")))
  echo readFile($(outside / path("inner") / path("keep2.txt")))

  # a missing directory: fine by default, an error with checkDir
  removeDir(tree)
  echo raisesFor(tree) == NameNotFound

  # an empty directory
  createDir(tree)
  removeDir(tree, checkDir = true)
  echo dirExists($tree)

  # tryRemoveFinalDir stays single-level: it refuses a non-empty directory
  echo tryRemoveFinalDir(outside) != Success
  echo dirExists($outside)

  removeDir(base, checkDir = true)
  echo dirExists($base)

try:
  main()
except:
  quit "tremovedir: unexpected error"
