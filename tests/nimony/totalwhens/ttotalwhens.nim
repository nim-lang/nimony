# `.feature: "totalWhens"`: a platform `when` becomes a Leng `whenplat` that the
# backend resolves, and sem checks every branch. The branch the backend takes
# must be the one the classic `when` of `mclassicplat` takes on this target.
import std/syncio
import deps/mclassicplat
{.feature: "totalWhens".}
{.feature: "assumeSync".}

type
  Res = object
    s: string

var destroyed = 0

proc `=destroy`(r: Res) =
  # silent: how many run depends on the platform
  if r.s.len > 0: inc destroyed

proc `=wasMoved`(r: var Res) =
  r.s = ""

proc os(): string =
  result = "?"
  when defined(windows):
    result = "windows"
  elif defined(linux) or defined(bsd):
    # a local with a destructor in a branch that may not be taken
    let r = Res(s: "linux-or-bsd")
    result = r.s
  elif defined(macosx):
    let r = Res(s: "macosx")
    result = r.s
  else:
    result = "other"

proc cpu(): int =
  result = 0
  when defined(amd64):
    result = 1
  elif defined(arm64):
    result = 2
  else:
    # nested: an `and` is spelled as a nested `when`
    when defined(posix):
      result = 3
    else:
      result = 3

proc counted(): int =
  # a loop and a `return` inside a branch
  result = 0
  when defined(windows) or defined(posix):
    for i in 0 ..< 4:
      inc result
    return result + 1
  else:
    result = 5

echo os() == classicOs()
echo cpu() == classicCpu()
echo counted()
