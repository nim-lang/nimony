when defined(posix):
  import std/[syncio, assertions]

  import deps/mdynlibs


  let s = iconvOpen("12", "12")
  let s2 = iconvOpen("12", "13")

  iconvClose(s)
  iconvClose(s2)

  proc fox() =
    let s = iconvOpen("12", "12")
    let s2 = iconvOpen("12", "13")
    iconvClose(s)
    iconvClose(s2)

  fox()

  block:
    let s = iconvOpen("12", "12")
    let s2 = iconvOpen("12", "13")
    iconvClose(s)
    iconvClose(s2)