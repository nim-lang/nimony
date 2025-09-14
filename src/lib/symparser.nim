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

proc isLocalName*(s: string): bool =
  var dots = 0
  for c in s:
    if c == '.': inc dots
  result = dots <= 1

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

type
  SplittedModulePath* = object
    dir*: string
    name*: string
    ext*: string

proc splitModulePath*(s: string): SplittedModulePath =
  # We diverge from `splitFile` here in that we consider the `.2.nif` part the extension, not just the `.nif` part.
  var i = s.len - 2
  while i >= 0 and s[i] notin {'/', '\\'}:
    dec i
  var d = i + 1 # find first dot (i can be -1 here!)
  while d < s.len and s[d] != '.':
    inc d
  result = SplittedModulePath(dir: substr(s, 0, i-1), name: substr(s, i+1, d-1), ext: substr(s, d))

proc changeModuleExt*(s, ext: string): string =
  let mp = splitModulePath(s)
  if ext.len > 0 and ext[0] != '.':
    result = mp.dir & "/" & mp.name & "." & ext
  else:
    result = mp.dir & "/" & mp.name & ext

when isMainModule:
  import std/[assertions]
  assert extractVersionedBasename("abc.12.Mod132a3bc") == "abc.12"
  assert extractVersionedBasename("abc.Mod132a3bc") == ""

  let sn = splitSymName("abc.12.Mod132a3bc")
  assert sn.name == "abc.12"
  assert sn.module == "Mod132a3bc"

  let mp = splitModulePath("abc/def.2.nif")
  assert mp.dir == "abc"
  assert mp.name == "def"
  assert mp.ext == ".2.nif"

  let mp2 = splitModulePath("def.2.nif")
  assert mp2.dir == "", mp2.dir
  assert mp2.name == "def", mp2.name
  assert mp2.ext == ".2.nif"

  let mp3 = splitModulePath("def")
  assert mp3.dir == "", mp3.dir
  assert mp3.name == "def", mp3.name
  assert mp3.ext == ""
