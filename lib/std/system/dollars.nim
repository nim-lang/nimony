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

func addFormatted[T](s: var string; x: T) {.untyped.} =
  ## Appends `x` to `s` without quoting: strings and chars are added as they
  ## are, every other value is added via `$`.
  when T is string or T is char:
    s.add x
  else:
    s.add $x

func `&`*[T: tuple](x: T): string =
  ## Concatenates the fields of the tuple `x` into a single string. Strings
  ## and chars are added unchanged, every other field is converted via `$`.
  ## This is Nimony's replacement for `strformat`:
  ##
  ##   ```nim
  ##   let x = 42
  ##   assert &("x is ", x, "!") == "x is 42!"
  ##   ```
  result = ""
  for value in fields(x):
    result.addFormatted value
