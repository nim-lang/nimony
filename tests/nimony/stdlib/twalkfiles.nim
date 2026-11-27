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
  var sep = "/"
  when defined(windows):
    sep = "\\"
  assert sorted(collect(baseStr & sep & sep & "*.txt")) == sorted(expected)
  assert collect(baseStr / "sub").len == 0
  assert collect(baseStr / "missing" / "*.txt").len == 0

  removeDir(base)
  echo "walkFiles: ok"

try:
  main()
except:
  quit "walkFiles test failed"
