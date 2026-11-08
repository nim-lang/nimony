# A raised `ref` object travels as `ref Exception`, and `except T` tests its
# type at run time, so its object needs RTTI. An object without an ancestor is
# final and has none (doc/language.md, "Object types"); neither has a `pure`
# root. Each such type is refused where it is named.
{.feature: "canraise".}

type
  Plain = ref object
    x: int
  Final {.final.} = ref object
    x: int
  PureRoot {.inheritable, pure.} = ref object
    x: int
  Gen[T] = ref object
    x: T
  Obj = object
    x: int
  RefObj = ref Obj

proc a() {.raises: Plain.} = discard
proc b() {.raises: Final.} = discard
proc c() {.raises: PureRoot.} = discard
proc d() {.raises: Gen[int].} = discard
proc e() {.raises: RefObj.} = discard

proc f() {.raises.} = raise IOError

proc g() =
  try: f()
  except Plain: discard
  except: discard

proc h() =
  try: f()
  except Gen[int] as x: discard
  except: discard

# `canraise` lets a routine without `.raises` raise; the raise is refused.
proc k() = raise Plain(x: 1)
