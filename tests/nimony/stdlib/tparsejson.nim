import std/[assertions, parsejson, streams, syncio]

proc events(s: string; rawStringLiterals = false): string {.raises.} =
  ## Drives the parser over `s` and renders the event stream compactly so the
  ## state machine can be asserted on without depending on `echo`.
  var p = default(JsonParser)
  open(p, newStringStream(s), "test", rawStringLiterals)
  result = ""
  while true:
    next(p)
    case p.kind
    of jsonError:
      result.add "E"
      break
    of jsonEof: break
    of jsonString: result.add "s:" & p.str & " "
    of jsonInt: result.add "i:" & $getInt(p) & " "
    of jsonFloat: result.add "f "
    of jsonTrue: result.add "true "
    of jsonFalse: result.add "false "
    of jsonNull: result.add "null "
    of jsonObjectStart: result.add "{ "
    of jsonObjectEnd: result.add "} "
    of jsonArrayStart: result.add "[ "
    of jsonArrayEnd: result.add "] "
  close(p)

proc main() {.raises.} =
  block: # scalars at top level
    assert events("123") == "i:123 "
    assert events("-7") == "i:-7 "
    assert events("3.5") == "f "
    assert events("true") == "true "
    assert events("false") == "false "
    assert events("null") == "null "
    assert events("\"hi\"") == "s:hi "

  block: # arrays
    assert events("[]") == "[ ] "
    assert events("[1, 2, 3]") == "[ i:1 i:2 i:3 ] "
    assert events("[true, [false], null]") == "[ true [ false ] null ] "

  block: # objects
    assert events("{}") == "{ } "
    assert events("""{"a": 1}""") == "{ s:a i:1 } "
    assert events("""{"a": 1, "b": [2]}""") == "{ s:a i:1 s:b [ i:2 ] } "

  block: # nesting
    assert events("""{"o": {"k": null}}""") == "{ s:o { s:k null } } "

  block: # whitespace and comments are skipped
    assert events("  [ 1 ,\n2 ]  ") == "[ i:1 i:2 ] "
    assert events("[1 /* c */, 2] // tail") == "[ i:1 i:2 ] "

  block: # string escapes decode to UTF-8
    assert events("\"\\u00e9\"") == "s:\xC3\xA9 " # é
    assert events("\"\\uD83D\\uDE00\"") == "s:\xF0\x9F\x98\x80 " # 😀
    assert events("\"a\\tb\"") == "s:a\tb "
    assert events("\"a\\\\b\"") == "s:a\\b "

  block: # rawStringLiterals keep the escapes verbatim
    assert events("\"\\u00e9 x\"", rawStringLiterals = true) == "s:\"\\u00e9 x\" "

  block: # malformed input yields an error event
    assert events("""{"x": }""") == "{ s:x E"
    assert events("[1 2]") == "[ i:1 E"
    assert events("\"unterminated") == "E"

  block: # errorMsg carries filename, position and reason
    var p = default(JsonParser)
    open(p, newStringStream("[1 2]"), "f.json")
    next(p) # [
    next(p) # 1
    next(p) # error
    assert p.kind == jsonError
    assert errorMsg(p) == "f.json(1, 4) Error: ']' expected"
    close(p)

  echo "ok"

try:
  main()
except:
  echo "unexpected exception"
