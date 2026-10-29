# Composite formatting for the built-in types.

func collectionToString[T: Stringable](x: openArray[T]; prefix, suffix: string): string =
  ## Shared implementation for the composite `$` overloads. `addQuoted`
  ## supplies the element quoting rules.
  result = prefix
  var first = true
  for value in x:
    if not first:
      result.add ", "
    else:
      first = false
    result.addQuoted value
  result.add suffix

func `$`*[T: Stringable](x: seq[T]): string =
  collectionToString(x, "@[", "]")

func `$`*[I, T: Stringable](x: array[I, T]): string =
  collectionToString(x, "[", "]")

func `$`*[T: Stringable](x: openArray[T]): string =
  collectionToString(x, "[", "]")

func `$`*[T: Ordinal and Stringable](x: set[T]): string =
  result = "{"
  var first = true
  for value in x:
    if not first:
      result.add ", "
    else:
      first = false
    result.addQuoted value
  result.add "}"

func `$`*[T: Stringable, U: Stringable](x: HSlice[T, U]): string =
  result = $x.a
  result.add " .. "
  result.add $x.b

func `$`*[T: tuple](x: T): string =
  result = "("
  var first = true
  var count = 0
  var named = false
  for name, value in fieldPairs(x):
    # `fieldPairs` invents `Field1`, `Field2`, ... for unnamed tuple fields;
    # suppress those so the output matches Nim's tuple syntax.
    let synthetic = name == "Field" & $(count + 1)
    inc count
    if not synthetic:
      named = true
    if not first:
      result.add ", "
    else:
      first = false
    if not synthetic:
      result.add name
      result.add ": "
    result.addQuoted value
  if count == 1 and not named:
    result.add ","
  result.add ")"
