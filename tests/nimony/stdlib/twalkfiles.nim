import std/[algorithm, assertions, dirs, os, paths, syncio]

proc collect(pattern: string): seq[string] =
  result = @[]
  for fn in walkFiles(pattern):
    result.add fn

proc sorted(files: seq[string]): seq[string] =
  result = files
  sort(result)

proc main {.raises.} =
  let base = path(getTempDir()) / path("nimony_walkfiles_test")
  removeDir(base)
  createDir(base)
  createDir(base / path("sub"))
  writeFile($(base / path("a.txt")), "a")
  writeFile($(base / path("b.txt")), "b")
  writeFile($(base / path("c.nim")), "c")
  writeFile($(base / path(".hidden.txt")), "h")
  writeFile($(base / path("sub") / path("d.nim")), "d")
  writeFile($(base / path("sub") / path("e.txt")), "e")

  let baseStr = $base
  var expected = @[baseStr / "a.txt", baseStr / "b.txt"]
  when defined(windows):
    expected.add baseStr / ".hidden.txt"
  assert sorted(collect(baseStr / "*.txt")) == sorted(expected)

  assert sorted(collect(baseStr / "*.nim")) == @[baseStr / "c.nim"]
  assert sorted(collect(baseStr / "?.txt")) ==
    sorted(@[baseStr / "a.txt", baseStr / "b.txt"])
  assert sorted(collect(baseStr / "sub" / "*.nim")) ==
    @[baseStr / "sub" / "d.nim"]
  assert sorted(collect(baseStr / ".*.txt")) == @[baseStr / ".hidden.txt"]
  assert sorted(collect(baseStr / "a.txt")) == @[baseStr / "a.txt"]
  when defined(windows):
    assert sorted(collect(baseStr & "\\\\*.txt")) == sorted(expected)
  else:
    # the pattern's directory is kept as written
    assert sorted(collect(baseStr & "//*.txt")) ==
      @[baseStr & "//a.txt", baseStr & "//b.txt"]
  assert collect(baseStr / "sub").len == 0
  assert collect(baseStr / "missing" / "*.txt").len == 0

  when not defined(windows):
    # a `*` in a file name is matched by a `*` in the pattern
    let star = baseStr / "star"
    createDir(path(star))
    writeFile(star / "*x.txt", "")
    writeFile(star / "a*b.txt", "")
    writeFile(star / "a.txt", "")
    assert sorted(collect(star / "*.txt")) ==
      @[star / "*x.txt", star / "a*b.txt", star / "a.txt"]
    assert sorted(collect(star / "a*.txt")) ==
      @[star / "a*b.txt", star / "a.txt"]

    # `..` after a symlink is resolved by the kernel, not folded lexically
    let fx = baseStr / "fx"
    let fy = baseStr / "fy"
    createDir(path(fx))
    createDir(path(fy / "inner"))
    writeFile(fx / "a.txt", "near")
    writeFile(fy / "a.txt", "far")
    writeFile(fy / "only.txt", "only")
    createSymlink("../fy/inner", fx / "lnfar")
    let far = fx & "/lnfar/../"
    let found = sorted(collect(far & "*.txt"))
    assert found == @[far & "a.txt", far & "only.txt"]
    assert readFile(found[0]) == "far"

  removeDir(base)
  echo "walkFiles: ok"

try:
  main()
except:
  quit "walkFiles test failed"
