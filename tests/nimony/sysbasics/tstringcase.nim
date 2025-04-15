import std/syncio

proc main() =
  let a = "abc"
  case a
  of "def", "ghi": echo "wrong"
  of "abc", "jkl": echo "correct"
  else: echo "also wrong"

main()

block arrayconstr:
  const md_extension = ".md"

  proc test(ext: string) =
    case ext
    of ".txt", r".markdown", md_extension:
      echo "Found!"
    else:
      echo "Not found!"

  test(".something")
  var foo = ".markdown"
  test(foo)
  test(".md")
