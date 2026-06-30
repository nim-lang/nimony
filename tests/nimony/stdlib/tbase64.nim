import std/[assertions, base64]

proc main =
  # RFC 4648 test vectors
  assert encode("") == ""
  assert encode("f") == "Zg=="
  assert encode("fo") == "Zm8="
  assert encode("foo") == "Zm9v"
  assert encode("foob") == "Zm9vYg=="
  assert encode("fooba") == "Zm9vYmE="
  assert encode("foobar") == "Zm9vYmFy"

  assert decode("") == ""
  assert decode("Zg==") == "f"
  assert decode("Zm8=") == "fo"
  assert decode("Zm9v") == "foo"
  assert decode("Zm9vYg==") == "foob"
  assert decode("Zm9vYmE=") == "fooba"
  assert decode("Zm9vYmFy") == "foobar"

  # round-trip incl. high bytes
  let data = "\x00\x01\x02\xff\xfe\x80hello, web!"
  assert decode(encode(data)) == data

  # URL-safe alphabet: bytes that map to + and / in standard become - and _
  let raw = "\xfb\xff\xbf"
  assert encode(raw) == "+/+/"
  assert encode(raw, safe = true) == "-_-_"
  # decode accepts both alphabets
  assert decode("-_-_") == raw
  assert decode("+/+/") == raw

  # whitespace in input is ignored on decode
  assert decode("Zm 9v") == "foo"
  assert decode("Zm9v\n") == "foo"

  # encode over bytes (0x66 0x6f 0x6f == "foo")
  assert encode(@[byte 102, 111, 111]) == "Zm9v"
  assert encode(@[byte 0xfb, 0xff, 0xbf], safe = true) == "-_-_"

  # encodeMime wraps into lines
  assert encodeMime("foobar", lineLen = 4) == "Zm9v\r\nYmFy"
  assert encodeMime("foobar") == "Zm9vYmFy"  # fits one line
  assert encodeMime("foobar", lineLen = 4, newLine = "\n") == "Zm9v\nYmFy"

main()
