import std/[assertions, files, os, parsecfg, paths, sequtils, streams, strutils,
            syncio, tables]

func sectionNames(dict: Config): seq[string] =
  result = newSeq[string]()
  for section in dict.sections:
    result.add(section)

proc main() {.raises.} =
  block: # creating, writing and reading a configuration
    var dict = newConfig()
    dict.setSectionKey("", "charset", "utf-8")
    dict.setSectionKey("Package", "name", "hello")
    dict.setSectionKey("Package", "--threads", "on")
    dict.setSectionKey("Author", "name", "nim-lang")
    dict.setSectionKey("Author", "website", "nim-lang.org")

    let text = $dict
    assert text == """charset=utf-8
[Package]
name=hello
--threads:on
[Author]
name=nim-lang
website=nim-lang.org
"""

    let parsed = loadConfig(newStringStream(text))
    assert parsed.getSectionValue("", "charset") == "utf-8"
    assert parsed.getSectionValue("Package", "--threads") == "on"
    assert parsed.getSectionValue("Package", "name") == "hello"
    assert parsed.getSectionValue("Author", "name") == "nim-lang"
    assert parsed.getSectionValue("Author", "website") == "nim-lang.org"
    assert parsed.getSectionValue("Author", "missing", "fallback") == "fallback"
    assert parsed.sectionNames() == @["", "Package", "Author"]
    assert toSeq(parsed.sections) == @["", "Package", "Author"]

  block: # event based parsing
    var p = CfgParser()
    let stream = newStringStream("""
# a comment
charset = "utf-8"
[Package]
name = "hello"
--threads:on
""")
    p.open(stream, "config.ini")

    var e = next(p)
    assert e.kind == cfgKeyValuePair
    assert e.key == "charset"
    assert e.value == "utf-8"

    e = next(p)
    assert e.kind == cfgSectionStart
    assert e.section == "Package"

    e = next(p)
    assert e.kind == cfgKeyValuePair
    assert e.key == "name"
    assert e.value == "hello"

    e = next(p)
    assert e.kind == cfgOption
    assert e.key == "threads"
    assert e.value == "on"

    e = next(p)
    assert e.kind == cfgEof
    assert p.getFilename() == "config.ini"
    assert p.getLine() > 0
    assert p.getColumn() >= 0
    p.close()

    var offsetParser = CfgParser()
    offsetParser.open(newStringStream("key=value"), "offset.ini", 10)
    assert offsetParser.getLine() == 11
    offsetParser.close()

  block: # quoted, raw and triple quoted strings
    let dict = loadConfig(newStringStream("""
[strings]
ordinary = "line1\nline2"
raw = r"line1\nline2"
triple = """ & "\"\"\"line1\nline2\"\"\"" & """
escaped = "a\"b"
hex = "\x41"
decimal = "\65"
"""))
    assert dict.getSectionValue("strings", "ordinary") == "line1\nline2"
    assert dict.getSectionValue("strings", "raw") == "line1\\nline2"
    assert dict.getSectionValue("strings", "triple") == "line1\nline2"
    assert dict.getSectionValue("strings", "escaped") == "a\"b"
    assert dict.getSectionValue("strings", "hex") == "A"
    assert dict.getSectionValue("strings", "decimal") == "A"

  block: # sections with spaces and special characters
    var dict = newConfig()
    dict.setSectionKey("", "--a:b", "value")
    dict.setSectionKey("", "email", "lihaifeng@wxm.com")
    dict.setSectionKey("Selection A", "key", "value")
    dict.setSectionKey("a:b", "key", "value")

    let text = $dict
    assert text.contains("[\"a:b\"]")
    assert text.contains("--\"a:b\":value")
    assert text.contains("email=\"lihaifeng@wxm.com\"")

    let parsed = loadConfig(newStringStream(text))
    assert parsed.getSectionValue("Selection A", "key") == "value"
    assert parsed.getSectionValue("a:b", "key") == "value"
    assert parsed.getSectionValue("", "--a:b") == "value"
    assert parsed.getSectionValue("", "email") == "lihaifeng@wxm.com"

  block: # modifying and deleting sections
    var dict = loadConfig(newStringStream("""
[Package]
name=hello
version=1
[Author]
name=nim-lang
email="nim@example.com"
"""))
    dict.setSectionKey("Package", "name", "changed")
    assert dict.getSectionValue("Package", "name") == "changed"
    dict.delSectionKey("Author", "email")
    assert dict.getSectionValue("Author", "email", "gone") == "gone"
    assert dict.getSectionValue("Author", "name") == "nim-lang"

    dict.delSectionKey("Package", "version")
    dict.delSectionKey("Package", "name")
    assert not dict.hasKey("Package")
    assert dict.sectionNames() == @["Author"]

    dict.setSectionKey("New", "key", "value")
    assert dict.hasKey("New")
    assert dict["New"]["key"] == "value"
    dict["New"]["key"] = "other"
    assert dict.getSectionValue("New", "key") == "other"
    assert dict.len() == 2

    var pairs = newSeq[string]()
    for section, values in dict.pairs:
      pairs.add(section & ":" & $values.len)
    assert pairs == @["Author:1", "New:1"]

    dict.delSection("Author")
    assert not dict.hasKey("Author")
    dict.clear()
    assert dict.len() == 0
    assert dict.sectionNames().len == 0

  block: # spaces, indentation, colon delimiters and values without a delimiter
    let dict = loadConfig(newStringStream("""
[Simple Values]
key=value
spaces in keys=allowed
spaces in values=allowed as well
spaces around the delimiter = obviously
you can also use : to delimit keys from values
[No Values]
key_without_value
# empty string value is not allowed =
[ Seletion A   ]
space around section name will be ignored
[Sections Can Be Indented]
    can_values_be_as_well = True
    purpose = formatting for readability
"""))
    assert dict.getSectionValue("Simple Values", "key") == "value"
    assert dict.getSectionValue("Simple Values", "spaces in keys") == "allowed"
    assert dict.getSectionValue("Simple Values", "spaces in values") ==
      "allowed as well"
    assert dict.getSectionValue("Simple Values", "spaces around the delimiter") ==
      "obviously"
    assert dict.getSectionValue("Simple Values", "you can also use") ==
      "to delimit keys from values"
    assert dict["No Values"].hasKey("key_without_value")
    assert not dict["No Values"].hasKey("# empty string value is not allowed")
    assert dict.getSectionValue("Seletion A",
      "space around section name will be ignored") == ""
    assert dict.getSectionValue("Sections Can Be Indented",
      "can_values_be_as_well") == "True"
    assert dict.getSectionValue("Sections Can Be Indented", "purpose") ==
      "formatting for readability"

  block: # parser errors are events, not exceptions
    var p = CfgParser()
    p.open(newStringStream("[section"), "bad.ini")
    var e = next(p)
    assert e.kind == cfgError
    assert e.msg.contains("']' expected")
    p.close()

    var p2 = CfgParser()
    p2.open(newStringStream("= value"), "bad2.ini")
    e = next(p2)
    assert e.kind == cfgError
    assert e.msg.contains("invalid token")
    p2.close()

    let partial = loadConfig(newStringStream("key=value\n=bad\nother=value"))
    assert partial.getSectionValue("", "key") == "value"
    assert partial.getSectionValue("", "other") == ""

  block: # diagnostics and ignored entries
    var p = CfgParser()
    p.open(newStringStream("key=value"), "messages.ini")
    let e = next(p)
    assert p.errorStr("boom") == "messages.ini(1, 9) Error: boom"
    assert p.warningStr("careful") == "messages.ini(1, 9) Warning: careful"
    assert p.ignoreMsg(e) == "messages.ini(1, 9) Warning: key ignored: key"
    p.close()

  block: # file overloads
    let filename = getTempDir() / "nimony_tparsecfg.ini"
    try:
      writeFile(filename, "charset=utf-8\n[Package]\nname=hello\n")
    except ErrorCode:
      quit "tparsecfg: cannot write test file"

    var dict = loadConfig(filename)
    assert dict.getSectionValue("Package", "name") == "hello"
    dict.setSectionKey("Package", "name", "changed")
    dict.writeConfig(filename)

    let reloaded = loadConfig(filename)
    assert reloaded.getSectionValue("Package", "name") == "changed"
    discard tryRemoveFile(path(filename))

  block: # a missing file raises IOError
    var err = Success
    try:
      discard loadConfig("/definitely/not/a/file.ini")
    except ErrorCode as e:
      err = e
    assert err == IOError

try:
  main()
except:
  quit "tparsecfg: unexpected exception"

echo "parsecfg: OK"
