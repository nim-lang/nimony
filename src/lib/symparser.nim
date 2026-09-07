#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Parses NIF symbols into their components.
##
## `sliceSymbol` is the one place that knows the grammar; everything else here
## is (for now) the older per-question scanners it replaces. See #2457: the
## goal is that a symbol is taken apart ONCE, into `nifcore.NifSymbol`, and
## that these scanners then have no callers left.

const
  Digits = {'0'..'9'}

type
  SymbolSlices* = object
    ## Where each component of a NIF symbol sits inside the string that spells
    ## it: `<name>.<disamb>`, `<name>.<disamb>.<module>` or
    ## `<name>.<disamb>.<dedup>.<module>`. The name always starts at 0, and a
    ## component the symbol does not have has a length of 0.
    nameLen*: int
    disamb*: int
    dedupStart*, dedupLen*: int
    moduleStart*, moduleLen*: int
    wellFormed*: bool
      ## False for an atom that is not a symbol at all -- no `.` followed by a
      ## digit anywhere in it, which is what an operator definition (`[]=`)
      ## looks like. Every other field is then 0 and the whole string is the
      ## name.

proc sliceSymbol*(s: string): SymbolSlices =
  ## Take a symbol apart without copying anything out of it.
  ##
  ## The name ends at the LAST `.` that is followed by a digit -- a name may
  ## well contain dots itself (`a.b.c.23`), only the disambiguator's dot ends
  ## it. What follows is split at dots: the first component is the
  ## disambiguator, the last (if there is another) is the module, and anything
  ## between them is the deduplication key of a generic instantiation. This is
  ## the same rule `nifreader`'s split-symbol mode applies to the raw bytes.
  result = SymbolSlices(nameLen: s.len, disamb: 0,
                        dedupStart: 0, dedupLen: 0,
                        moduleStart: 0, moduleLen: 0, wellFormed: false)
  var i = s.len - 2
  while i > 0:
    if s[i] == '.' and s[i+1] in Digits: break
    dec i
  if i <= 0: return

  result.wellFormed = true
  result.nameLen = i
  let restStart = i+1
  var j = restStart
  while j < s.len and s[j] in Digits:
    result.disamb = result.disamb * 10 + (ord(s[j]) - ord('0'))
    inc j

  var firstDot = -1
  var k = restStart
  while k < s.len:
    if s[k] == '.':
      firstDot = k
      break
    inc k
  if firstDot < 0: return # a local symbol: no module, no dedup key

  var lastDot = firstDot
  k = firstDot+1
  while k < s.len:
    if s[k] == '.': lastDot = k
    inc k
  if lastDot != firstDot:
    result.dedupStart = firstDot+1
    result.dedupLen = lastDot - (firstDot+1)
  result.moduleStart = lastDot+1
  result.moduleLen = s.len - (lastDot+1)


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

proc genericTypeName*(key, modname: string): string =
  result = "`t.0.I" & key & "." & modname

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

proc `$`*(s: SplittedSymName): string =
  if s.module.len > 0:
    result = s.name & "." & s.module
  else:
    result = s.name

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

proc derivedName*(stem, tag: string): string =
  ## The `identifier.<number>` half of a symbol the compiler mints ALONGSIDE
  ## another one — a closure's environment type, a class's vtable, a coroutine's
  ## frame. `stem` is the originating symbol minus its module suffix, and the
  ## caller appends the module it wants the result to live in:
  ##
  ##   derivedName("outer.0", "env")        == "outer`env.0"
  ##   derivedName("gen.12.Iaaaa", "coro")  == "gen.12.Iaaaa`coro.0"
  ##
  ## The tag goes INTO the identifier rather than becoming a dotted segment of
  ## its own, because the two shapes say different things. nif-spec.md gives a
  ## global symbol as `<ident>.<disamb>.<moduleSuffix>` OR
  ## `<ident>.<disamb>.<key>.<moduleSuffix>`, "where `key` usually is the result
  ## from a generic instantiation". The `key` slot answers WHICH instantiation of
  ## `<ident>.<disamb>` this is — and because every module needing that
  ## instantiation derives the same key independently, `<ident>.<disamb>.<key>` is
  ## meaningful across module boundaries. That is exactly what lets a backend
  ## collapse the copies each importing module emits: DCE's
  ## `resolveSymbolConflicts`, `lengcgen`'s content-hashed
  ## `strlit.0.I<hash>.<mod>`, and nifasm's COMDAT merge all key on it.
  ##
  ## `env`, `coro`, `vt` are not keys — they name a ROLE, and the entity they name
  ## is private to one module. Put in the key slot they promise a cross-module
  ## identity they do not have, and two modules that each close over a variable in
  ## a proc named `outer` both claim `outer.0.env`. That is not a hypothetical:
  ## one module's closure read its captures out of the other's layout — see
  ## tests/nimony/closures/tenv_name_clash.nim.
  ##
  ## The backtick keeps the result out of the Nim-spellable namespace, matching
  ## the `` `f `` of a lifted local. It is inserted before the trailing version so
  ## the disambiguation number keeps its place; a stem that does not end in one
  ## (a keyed stem ends in its key) gets a fresh `.0` instead, which keeps both
  ## distinguishing parts — stem and tag — inside the identifier where they
  ## belong.
  ##
  ## Only a version at the very END counts, and that restriction is the whole
  ## point rather than an implementation detail: the result must be an UNKEYED
  ## global symbol. Scanning back past a later segment to find a number would
  ## leave that segment sitting in the key slot — `("gen.12.Iaaaa", "coro")` would
  ## come back as `gen`coro.12.Iaaaa`, a keyed name again, and one that has
  ## silently adopted the ORIGINAL symbol's key as its own cross-module identity.
  var i = stem.len - 1
  while i > 0 and stem[i] in {'0'..'9'}: dec i
  if i > 0 and i < stem.len - 1 and stem[i] == '.':
    result = substr(stem, 0, i-1) & "`" & tag & substr(stem, i)
  else:
    result = stem & "`" & tag & ".0"

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

proc splitLocalSymName*(s: string; basename: var string;
                        disamb: var int): bool =
  ## Splits a local symbol such as `tmp.14` into `tmp` and `14`.
  basename = ""
  disamb = 0
  var dot = s.len - 1
  while dot >= 0 and s[dot] in {'0'..'9'}:
    dec dot
  if dot <= 0 or dot == s.len - 1 or s[dot] != '.':
    return false
  for i in 0 ..< dot:
    if s[i] == '.':
      return false
  var value = 0
  for i in dot + 1 ..< s.len:
    let digit = ord(s[i]) - ord('0')
    if value > (high(int) - digit) div 10:
      return false
    value = value * 10 + digit
  basename = substr(s, 0, dot - 1)
  disamb = value
  result = true

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
  result = mp.dir
  if result.len > 0: result.add "/"
  result.add mp.name
  if ext.len > 0 and ext[0] != '.':
    result.add "." & ext
  else:
    result.add ext

proc `$`*(s: SplittedModulePath): string =
  result = s.dir
  if result.len > 0: result.add "/"
  result.add s.name
  result.add s.ext

when isMainModule:
  import std/[assertions]

  proc hasDot(s: string): bool =
    result = false
    for c in s:
      if c == '.': return true

  # `sliceSymbol` answers every question the scanners below answer, and must
  # answer them the same way -- that is what lets the scanners go away.
  proc agrees(s: string) =
    let sl = sliceSymbol(s)
    var isGlobal = false
    let base = extractBasename(s, isGlobal)
    if sl.wellFormed:
      assert substr(s, 0, sl.nameLen-1) == base, s
      assert substr(s, sl.moduleStart, sl.moduleStart+sl.moduleLen-1) == extractModule(s), s
      # `isLocalName` counts dots instead of parsing, so it only agrees for a
      # name without dots -- for `Pool.Obj.0` or `..<.3` it says "not local"
      # about a symbol that has no module at all. Real symbols have such names
      # (`dollar`.CaseMode`, `Pool.Obj`, `..<`), so this is a divergence to
      # settle at the call sites, not to paper over here.
      if not hasDot(substr(s, 0, sl.nameLen-1)):
        assert (sl.moduleLen == 0) == isLocalName(s), s
      assert isInstantiation(s) ==
        (sl.dedupLen > 0 and s[sl.dedupStart] == 'I'), s
      assert substr(s, 0, sl.nameLen) & $sl.disamb == extractVersionedBasename(s), s
    else:
      assert base == "", s
      # `extractModule` does answer for such an atom -- it says "bar" for
      # `foo.bar` -- because it looks for a trailing dotted segment without
      # first establishing that there is a disambiguator. There is no symbol
      # here to have a module, so `sliceSymbol` says so instead.

  for s in ["abc.12.Mod132a3bc", "abc.12", "a.b.c.23", "abc.12.Iabcdefghi.mod2",
            "tmp.14", "outer`env.0.mymod", "gen.12.Iaaaa`coro.0.mymod",
            "[]=", "foo.bar", "x.0"]:
    agrees s

  let ls = sliceSymbol("abc.12.Ikey.mod")
  assert ls.nameLen == 3
  assert ls.disamb == 12
  assert substr("abc.12.Ikey.mod", ls.dedupStart, ls.dedupStart+ls.dedupLen-1) == "Ikey"
  assert substr("abc.12.Ikey.mod", ls.moduleStart, ls.moduleStart+ls.moduleLen-1) == "mod"
  let lt = sliceSymbol("tmp.14")
  assert lt.wellFormed and lt.nameLen == 3 and lt.disamb == 14
  assert lt.dedupLen == 0 and lt.moduleLen == 0
  let lo = sliceSymbol("[]=")
  assert not lo.wellFormed and lo.nameLen == 3
  assert extractVersionedBasename("abc.12.Mod132a3bc") == "abc.12"
  assert extractVersionedBasename("abc.Mod132a3bc") == ""

  let sn = splitSymName("abc.12.Mod132a3bc")
  assert sn.name == "abc.12"
  assert sn.module == "Mod132a3bc"

  assert derivedName("outer.0", "env") == "outer`env.0"
  assert derivedName("abc.12", "vt") == "abc`vt.12"
  # An instantiation stem ends in its key, not in a version: the tag and a fresh
  # number are appended so the key stays inside the identifier.
  assert derivedName("gen.12.Iaaaa", "coro") == "gen.12.Iaaaa`coro.0"
  # Whatever a caller appends its module to, the result must NOT read back as an
  # instantiation — that is the whole point of the exercise.
  assert not isInstantiation(derivedName("outer.0", "env") & ".mymod")
  assert not isInstantiation(derivedName("gen.12.Iaaaa", "coro") & ".mymod")
  assert isInstantiation("gen.12.Iaaaa.mymod")
  # ...and the module suffix must still be recoverable.
  assert extractModule(derivedName("gen.12.Iaaaa", "coro") & ".mymod") == "mymod"
  assert extractModule(derivedName("outer.0", "env") & ".mymod") == "mymod"

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

  var basename = ""
  var disamb = 0
  assert splitLocalSymName("tmp.14", basename, disamb)
  assert basename == "tmp"
  assert disamb == 14
  assert not splitLocalSymName("tmp.14.mod", basename, disamb)
  assert not splitLocalSymName("tmp.part.14", basename, disamb)
