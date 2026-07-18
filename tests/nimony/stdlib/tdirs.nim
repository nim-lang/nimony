import std/[assertions, dirs, os, syncio]

proc main =
  block:
    # issue #2159
    let testdir = getTempDir() / "nimony_test_dir"
    try:
      createDir(path(testdir))
    except:
      quit "createDir test failed"

main()
