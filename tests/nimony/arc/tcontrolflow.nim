import std/[syncio, assertions]

type
  Foo = object
    id: int

proc `=destroy`(x: var Foo) =
  if x.id != 0:
    echo "destroyed"
    x.id = 0

proc construct(): Foo =
  Foo(id: 3)

proc elifIsEasy(cond: bool) =
  echo "begin A"
  if cond:
    echo "if"
  elif construct().id == 3:
    echo "elif"
  else:
    echo "else"
  echo "end A"

elifIsEasy(false)

proc orIsHard(cond: bool) =
  echo "begin ", cond
  if cond or construct().id == 3:
    echo "if"
  else:
    echo "else"
  echo "end ", cond

orIsHard(false)
orIsHard(true)

type
  Control = ref object
    x: int

  MouseEvent = ref object
    control: Control
    button: int

proc run(data: Control) =
  var evt = MouseEvent(button: 1)
  evt.control = data
  if evt.button == 1:
    discard
  else:
    return

  echo data.x

let c = Control(x: 7)
run(c)

proc sysFatal(message: string) =
  var buf = newStringOfCap(200)
  add(buf, "##")
  add(buf, message)
  add(buf, "##")
  echo buf

proc ifexpr(i, a, b: int) =
  sysFatal(
    if b < a: "index out of bounds, the container is empty"
    else: "index " & $i & " not in " & $a & " .. " & $b
  )

ifexpr(2, 0, 1)

template toSeq(): untyped =
  block:
    var result = @[1]
    result

proc clItems(s: seq[int]) =
  assert s.len == 1

proc escapeCheck =
  clItems(toSeq())

escapeCheck()

proc seqsEqual(a, b: string): bool =
  if false:
    false
  else:
    (var result1 = a; result1) == (var result2 = b; result2)

let expected = "hello"
echo seqsEqual(expected, expected)
