import std/[assertions, files, os, ropes, streams, syncio]

proc main() {.raises.} =
  let empty = rope("")
  assert empty == nil
  assert empty.len == 0
  assert $empty == ""

  let hello = rope("Hello, ")
  let nim = rope("Nim!")
  let combined = hello & nim
  assert combined.len == 11
  assert $combined == "Hello, Nim!"
  assert combined[0] == 'H'
  assert combined[7] == 'N'
  assert combined[22] == '\0'
  let negative = -1
  assert combined[negative] == '\0'

  assert $(hello & "Nim!") == "Hello, Nim!"
  assert $("Hello, " & nim) == "Hello, Nim!"
  assert $(&[hello, nim]) == "Hello, Nim!"

  var leaves = 0
  for s in combined.leaves:
    if leaves == 0:
      assert s == "Hello, "
    else:
      assert s == "Nim!"
    inc leaves
  assert leaves == 2

  var chars = 0
  for ch in combined:
    assert ch == "Hello, Nim!"[chars]
    inc chars
  assert chars == combined.len

  var added = rope("Hello, ")
  added.add(nim)
  assert $added == "Hello, Nim!"
  added.add("!")
  assert $added == "Hello, Nim!!"

  assert $rope(429) == "429"
  assert $rope(4.5) == "4.5"

  let formatted = "$1 $2 $3" % [rope("Nim"), rope("is"), rope("a great language")]
  assert $formatted == "Nim is a great language"
  assert $("$# $# $#" % [rope("Nim"), rope("is"), rope("a great language")]) ==
    "Nim is a great language"
  assert $("${1} ${2} ${3}" % [rope("Nim"), rope("is"), rope("a great language")]) ==
    "Nim is a great language"

  var prefixed = rope("Dash: ")
  prefixed.addf "$1 $2 $3", [rope("Nim"), rope("is"), rope("a great language")]
  assert $prefixed == "Dash: Nim is a great language"

  let ss = newStringStream()
  write(ss, combined)
  assert ss.data == "Hello, Nim!"

  let name = getTempDir() / "nimony_tropes.txt"
  var outFile: File
  assert open(outFile, name, fmWrite)
  write(outFile, combined)
  close(outFile)

  assert combined.equalsFile(name)
  assert not rope("Hello, Nim?").equalsFile(name)
  var inFile: File
  assert open(inFile, name)
  assert combined.equalsFile(inFile)
  close(inFile)

  discard tryRemoveFile(path(name))

  enableCache()
  let cached1 = rope("cached rope")
  let cached2 = rope("cached rope")
  assert cached1 == cached2
  assert $cached1 == "cached rope"
  assert $cached2 == "cached rope"
  disableCache()

  echo "ropes: OK"

try:
  main()
except:
  quit "tropes: unexpected error"
