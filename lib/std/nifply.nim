import std/assertions
import ../../src/lib/[nifbuilder, nifreader]

export close, extract

proc internalTypeName*[T](x: typedesc[T]): string {.magic: "InternalTypeName", noSideEffect.}

iterator internalFieldPairs*[T: tuple|object](x: T): tuple[key: string, val: untyped] {.
  magic: "InternalFieldPairs", noSideEffect.}

type
  NifBuilder* = Builder

proc nifBuilderOpen*(filename: string; compact = false): Builder =
  nifbuilder.open(filename, compact)

proc nifBuilderOpen*(sizeHint: int; compact = false): Builder =
  nifbuilder.open(sizeHint, compact)

proc toNif*(b: var NifBuilder; x: string) =
  b.addStrLit x

proc toNif*(b: var NifBuilder; x: int) =
  b.addIntLit x

proc toNif*(b: var NifBuilder; x: uint) =
  b.addUIntLit x

proc toNif*(b: var NifBuilder; x: float) =
  b.addFloatLit x

proc toNif*(b: var NifBuilder; x: bool) =
  b.addKeyw($x)

proc toNif*[E: enum](b: var NifBuilder; x: E) =
  b.withTree "conv":
    b.addSymbol internalTypeName(E)
    b.addUIntLit uint(x)

proc toNif*[O: object](b: var NifBuilder; x: O) =
  b.withTree "oconstr":
    b.addSymbol internalTypeName(O)
    for name, f in internalFieldPairs(x):
      b.withTree "kv":
        b.addSymbol name
        toNif b, f

type
  NifReader* = Reader

proc nifReaderOpen*(filename: string): Reader =
  nifreader.open(filename)

proc nifReaderOpenFromBuffer*(buf: sink string): Reader =
  nifreader.openFromBuffer(buf)

template expectTree(r: var NifReader; tag: string; body: untyped) =
  let t = r.next
  assert t.tk == ParLe
  assert t.s == tag
  body
  let t2 = r.next
  assert t2.tk == ParRi

proc expectSymbol(r: var NifReader; sym: string) =
  let t = r.next
  assert t.tk == Symbol
  assert t.s == sym

proc fromNif*[T: not typedesc](r: var NifReader; x: var T) {.untyped, inline.} =
  x = r.fromNif T

proc fromNif*(r: var NifReader; t: typedesc[string]): string =
  let t = r.next
  assert t.tk == StringLit
  result = $t.s

proc fromNif*(r: var NifReader; t: typedesc[int]): int =
  let t = r.next
  result = int decodeInt t

proc fromNif*(r: var NifReader; t: typedesc[uint]): uint =
  let t = r.next
  result = uint decodeUInt t

proc fromNif*(r: var NifReader; t: typedesc[float]): float =
  let t = r.next
  result = decodeFloat t

proc fromNif*(r: var NifReader; t: typedesc[bool]): bool =
  let t = r.next

  if t.s == "true":
    result = true
  elif t.s == "false":
    result = false
  else:
    assert false, "got unexpected token: " & $t.s

  let t2 = r.next
  assert t2.tk == ParRi

proc fromNif*[E: enum](r: var NifReader; t: typedesc[E]): E =
  r.expectTree "conv":
    r.expectSymbol internalTypeName E
    let val = r.next
    result = E decodeUInt val

proc fromNif*[O: object](r: var NifReader; t: typedesc[O]): O =
  r.expectTree "oconstr":
    r.expectSymbol internalTypeName O
    for name, f in internalFieldPairs(result):
      r.expectTree "kv":
        r.expectSymbol name
        r.fromNif f
