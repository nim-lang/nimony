import plugins
import "../../../../src/lib/cparser_nimony"

# Touch this wrapper when validating import-driven lowerer changes. Current
# Nimony plugin caching does not reliably notice transitive imports. Cache bump: 9.

proc describeNode(n: NifCursor): string =
  result = $n.kind
  if n.kind == ParLe:
    result.add " tag="
    result.add n.tagText

proc takeStringArg(n: NifCursor): tuple[value: string, info: LineInfo, ok: bool] =
  case n.kind
  of StringLit:
    (n.stringValue, n.info, true)
  of ParLe:
    if n.tagText == "suf":
      var it = n
      it = firstChild(it)
      if it.kind == StringLit:
        (it.stringValue, it.info, true)
      elif it.kind == ParLe and it.tagText == "par":
        it = firstChild(it)
        takeStringArg(it)
      else:
        ("", NoLineInfo, false)
    elif n.tagText == "par":
      var it = n
      it = firstChild(it)
      takeStringArg(it)
    else:
      ("", NoLineInfo, false)
  else:
    ("", NoLineInfo, false)

proc parseArgs(n: NifCursor): tuple[header, source: string, sourceInfo: LineInfo, ok: bool, err: string] =
  result = ("", "", NoLineInfo, false, "")
  var n = templateArgs(n)

  let headerArg = takeStringArg(n)
  if not headerArg.ok:
    return ("", "", NoLineInfo, false,
      "first plugin argument must be a string literal header name; got " & describeNode(n))
  result.header = headerArg.value
  skip n

  let sourceArg = takeStringArg(n)
  if not sourceArg.ok:
    return ("", "", NoLineInfo, false,
      "second plugin argument must be a string literal C snippet; got " & describeNode(n))
  result.source = sourceArg.value
  result.sourceInfo = sourceArg.info
  skip n

  if n.kind != ParRi:
    return ("", "", NoLineInfo, false,
      "unexpected extra plugin arguments; next is " & describeNode(n))

  result.ok = true

proc tr(n: NifCursor): NifBuilder =
  let args = parseArgs(n)
  if not args.ok:
    return errorTree(args.err, n)
  result = parseCBindingsToNimony(args.source,
    NimonyBindingsConfig(
      header: args.header,
      exportSymbols: false,
      originInfo: args.sourceInfo
    ))

let input = loadPluginInput()
saveTree tr(input)
