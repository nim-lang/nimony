import std/[dirs, paths, syncio, assertions]

# Regression test: createDir must create a NESTED directory structure in one
# call (its documented behaviour). It used to walk parentDirs leaf-first, so
# the first mkdir hit ENOENT and raised before any parent was created.

proc main {.raises.} =
  let base = path("tcreatedir_nested_dir")
  removeDir(base)        # clean up a stale run; ignore "does not exist"

  let nested = base / path("a") / path("b")
  createDir(nested)      # must create base, base/a and base/a/b

  # prove the leaf exists by creating a file inside it
  writeFile($(nested / path("probe.txt")), "ok")
  var found = 0
  for kind, p in walkDir(nested, relative = true):
    if kind == pcFile: inc found
  assert found == 1

  # relative nested creation below an existing dir also works
  createDir(base / path("x") / path("y"))

  # leaf-up cleanup (removeDir removes a single, empty directory)
  removeFile(nested / path("probe.txt"))
  removeDir(nested)
  removeDir(base / path("a"))
  removeDir(base / path("x") / path("y"))
  removeDir(base / path("x"))
  removeDir(base)
  echo "ok"

try:
  main()
except:
  quit "createDir nested test failed"
