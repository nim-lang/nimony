import ../../src/lib/nifbuilder

export open, close, extract

proc internalTypeName*[T](x: typedesc[T]): string {.magic: "InternalTypeName", noSideEffect.}

iterator internalFieldPairs*[T: tuple|object](x: T): tuple[key: string, val: untyped] {.
  magic: "InternalFieldPairs", noSideEffect.}

type
  NifBuilder* = Builder

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
  discard
