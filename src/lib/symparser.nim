#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Parses NIF symbols into their components.

proc extractBasename*(s: string; isGlobal: var bool): string =
  # From "abc.12.Mod132a3bc" extract "abc".
  # From "abc.12" extract "abc".
  # From "a.b.c.23" extract "a.b.c".
  var i = s.len - 2
  while i > 0:
    if s[i] == '.':
      if s[i+1] in {'0'..'9'}:
        return substr(s, 0, i-1)
      isGlobal = true # we skipped one dot so it's a global name
    dec i
  return ""

proc extractBasename*(s: var string) =
  var i = s.len - 2
  while i > 0:
    if s[i] == '.':
      if s[i+1] in {'0'..'9'}:
        s.setLen i
        return
    dec i

proc extractModule*(s: string): string =
  # From "abc.12.Mod132a3bc" extract "Mod132a3bc".
  # From "abc.12" extract "".
  var i = s.len - 2
  while i > 0:
    if s[i] == '.':
      if s[i+1] in {'0'..'9'}:
        return ""
      else:
        return substr(s, i+1)
    dec i
  return ""

type
  SplittedSymName* = object
    name*: string
    module*: string

proc splitSymName*(s: string): SplittedSymName =
  var i = s.len - 2
  while i > 0:
    if s[i] == '.':
      if s[i+1] in {'0'..'9'}:
        return SplittedSymName(name: s, module: "")
      else:
        return SplittedSymName(name: substr(s, 0, i-1), module: substr(s, i+1))
    dec i
  return SplittedSymName(name: s, module: "")

proc extractVersionedBasename*(s: string): string =
  # From "abc.12.Mod132a3bc" extract "abc.12".
  var i = s.len - 2
  while i > 0:
    if s[i] == '.':
      if s[i+1] in {'0'..'9'}:
        var j = i+1
        while j < s.len and s[j] in {'0'..'9'}: inc j
        return substr(s, 0, j-1)
    dec i
  return ""

proc isInstantiation*(s: string): bool =
  # abc.12.Iabcdefghi.mod2
  var i = s.len - 2
  var dots = 3
  while i > 0:
    if s[i] == '.':
      dec dots
      if s[i+1] in {'0'..'9'}:
        return dots == 0
      elif dots == 1 and s[i+1] != 'I':
        return false
    dec i
  result = false

proc removeModule*(s: string): string =
  # From "abc.12.Mod132a3bc" extract "abc.12".
  # From "abc.12" extract "abc.12".
  var i = s.len - 2
  while i > 0:
    if s[i] == '.':
      if s[i+1] in {'0'..'9'}:
        return s
      else:
        return substr(s, 0, i-1)
    dec i
  return s

when isMainModule:
  import std/[assertions]
  assert extractVersionedBasename("abc.12.Mod132a3bc") == "abc.12"
  assert extractVersionedBasename("abc.Mod132a3bc") == ""

  let sn = splitSymName("abc.12.Mod132a3bc")
  assert sn.name == "abc.12"
  assert sn.module == "Mod132a3bc"
