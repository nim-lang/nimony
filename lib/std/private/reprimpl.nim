# Private implementation of `std/repr`, following Nim's `system/repr_v2`
# semantics.
#
# The public module only exposes `repr`; the helper functions stay exported
# here so the `deps/repr` plugin's `bindSym` calls can name them. They
# deliberately recurse through `reprImpl`, which lets recursive types
# terminate via generic instantiation.

template reprImpl*[T](x: typedesc[T]): string {.untyped.} =
  reprTypedesc(x)

template reprImpl*[T](x: T): untyped {.untyped.} =
  reprTyped(typeof(x), x)

template reprTyped[T](t: typedesc[T], x: T): string {.plugin: "../deps/repr".}
template reprTypedesc[T](x: typedesc[T]): string {.plugin: "../deps/repr".}

func reprChar*(x: char): string =
  result = "'"
  if x in {'\\', '\0'..'\31', '\127'..'\255'}:
    result.add '\\'
  if x in {'\0'..'\31', '\127'..'\255'}:
    result.add $ord(x)
  else:
    result.add x
  result.add '\''

func addReprStringChar(result: var string; c: char) =
  if c in {'"', '\\', '\0'..'\31', '\127'..'\255'}:
    result.add '\\'
  case c
  of '\n':
    result.add "n\n"
  of '\0'..'\9', '\11'..'\31', '\127'..'\255':
    result.add $ord(c)
  else:
    result.add c

func reprString*(x: string): string =
  result = "\""
  for c in x:
    addReprStringChar(result, c)
  result.add '"'

func reprCstring*(x: nil cstring): string =
  result = "\""
  if x != nil:
    for i in 0..<x.len:
      addReprStringChar(result, x[i])
  result.add '"'

func reprPointer*(x: nil pointer): string =
  const HexChars = "0123456789ABCDEF"
  if x == nil:
    return "nil"
  let n = cast[uint](x)
  result = newString(sizeof(pointer) * 2)
  var v = n
  for j in countdown(result.high, 0):
    result[j] = HexChars[int(v and 0xF)]
    v = v shr 4

func reprProc*[T](x: T): string {.untyped.} =
  # Nim's `repr` prints a routine as the address of its first word. For a
  # closure that word is the code pointer, matching `repr_v2`.
  reprPointer(cast[ptr pointer](addr x)[])

func reprInt*[T](x: T): string {.untyped.} = $int64(x)
func reprUInt*[T](x: T): string {.untyped.} = $uint64(x)
func reprFloat*[T](x: T): string {.untyped.} = $x
func reprBool*[T](x: T): string {.untyped.} = $x
func reprEnum*[T](x: T): string {.untyped.} = $x

func reprCollectionImpl[T](x: openArray[T]; prefix, separator,
                            suffix: string): string {.untyped.} =
  result = prefix
  var addSep = false
  for value in x:
    if addSep:
      result.add separator
    else:
      addSep = true
    result.add reprImpl(value)
  result.add suffix

func reprSeq*[T](x: seq[T]): string {.untyped.} =
  reprCollectionImpl(x, "@[", ", ", "]")

func reprArray*[I, T](x: array[I, T]): string {.untyped.} =
  reprCollectionImpl(x, "[", ", ", "]")

func reprOpenArray*[T](x: openArray[T]): string {.untyped.} =
  reprCollectionImpl(x, "[", ", ", "]")

func reprSet*[T](x: T): string {.untyped.} =
  result = "{"
  var addSep = false
  for value in x:
    if addSep:
      result.add ", "
    else:
      addSep = true
    result.add reprImpl(value)
  result.add "}"

func reprUncheckedArray*[T](x: T): string {.untyped.} =
  discard x
  "[...]"

func reprSlice*[T](x: T): string {.untyped.} =
  reprImpl(x.a) & " .. " & reprImpl(x.b)

func reprTupleNamed*[T](x: T): string {.untyped.} =
  result = "("
  var addSep = false
  for name, value in fieldPairs(x):
    if addSep:
      result.add ", "
    else:
      addSep = true
    result.add name
    result.add ": "
    result.add reprImpl(value)
  result.add ')'

func reprTuplePositional*[T](x: T): string {.untyped.} =
  result = "("
  var count = 0
  for name, value in fieldPairs(x):
    if count > 0:
      result.add ", "
    inc count
    result.add reprImpl(value)
  if count == 1:
    result.add ','
  result.add ')'

func reprObject*[T](x: T; typeName: string): string {.untyped.} =
  result = typeName
  result.add '('
  var branch = ""
  var fields = ""
  var addSep = false
  for name, value in fieldPairs(x):
    if name == "`kind":
      branch = reprImpl(value)
    else:
      if addSep:
        fields.add ", "
      else:
        addSep = true
      fields.add name
      fields.add ": "
      fields.add reprImpl(value)
  if branch.len > 0:
    result.add branch
    result.add '('
  result.add fields
  if branch.len > 0:
    result.add ')'
  result.add ')'

func reprRefObject*[T](x: nil ref T; typeName: string): string {.untyped.} =
  if x == nil:
    "nil"
  else:
    reprObject(x[], typeName)

func reprPtrObject*[T](x: nil ptr T; typeName: string): string {.untyped.} =
  if x == nil:
    "nil"
  else:
    reprObject(x[], typeName)

func reprRef*[T](x: nil ref T): string {.untyped.} =
  if x == nil:
    "nil"
  else:
    "ref " & reprImpl(x[])

func reprPtr*[T](x: nil ptr T): string {.untyped.} =
  if x == nil:
    "nil"
  else:
    "ptr " & reprImpl(x[])

func reprBase*[T; U](x: T; base: typedesc[U]): string {.untyped.} =
  reprImpl(cast[U](x))

func reprAnum*[T; U; Names](x: T; base: typedesc[U]; names: Names): string {.untyped.} =
  let idx = int(cast[U](x))
  var i = 0
  result = ""
  for field, name in fieldPairs(names):
    if idx == i:
      result = name
    inc i
