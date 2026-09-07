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
      ## The leading digits of the disambiguator. Meaningless unless
      ## `disambIsNumeric` -- see it.
    disambStart*, disambLen*: int
    disambIsNumeric*: bool
      ## Whether the disambiguator is a plain number. It is NOT always: the
      ## inliners mint `returnLabel.0h3` / `p.0i7` / `x.0d2` (a per-pass letter
      ## keeps hexer-, intra- and dce2-minted names from colliding), and arkham
      ## reserves `.sys.` / `.c.` for its syprocs and extprocs. `disamb` alone
      ## does not identify such a symbol -- `p.0h107` and `p.0h2` both read as
      ## 0 -- so anything that RECONSTRUCTS a symbol must use the text.
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
  ## The module is the LAST dot-separated component -- unless it STARTS with a
  ## digit, which makes it the disambiguator and the symbol local. That question
  ## needs no anchor, which is what makes it answerable for the symbols arkham
  ## mints with a reserved non-numeric disambiguator (`_exit.sys.sysvq0asl`,
  ## `write.c.sysvq0asl`).
  ##
  ## "Starts with a digit" rather than "is a number" is deliberate and is what
  ## every scanner here has always done: `p.0h107` is a LOCAL symbol whose
  ## disambiguator the inliner minted, not a symbol from a module called
  ## `0h107`. The flip side is that a module whose suffix begins with a digit
  ## cannot be told apart from a disambiguator -- a hazard that predates this
  ## and that only the file naming avoids.
  ##
  ## The name/disambiguator boundary DOES need the anchor -- the last `.`
  ## followed by a digit -- because a name may contain dots itself
  ## (`Pool.Obj.0`, `a.b.c.23`) and only the digit tells the two apart. A symbol
  ## with a reserved disambiguator therefore reports `wellFormed = false`, with
  ## everything but the module counted as the name, which is what
  ## `splitSymName` has always answered for it.
  result = SymbolSlices(nameLen: s.len, disamb: 0,
                        disambStart: 0, disambLen: 0, disambIsNumeric: false,
                        dedupStart: 0, dedupLen: 0,
                        moduleStart: 0, moduleLen: 0, wellFormed: false)
  var lastDot = -1
  var k = 0
  while k < s.len:
    if s[k] == '.': lastDot = k
    inc k

  var head = s.len ## everything that is not the module suffix
  if lastDot >= 0 and lastDot+1 < s.len and s[lastDot+1] notin Digits:
    result.moduleStart = lastDot+1
    result.moduleLen = s.len - (lastDot+1)
    head = lastDot
  elif lastDot == s.len-1:
    # a trailing dot is the "my own module" shorthand, which the reader expands
    # before anyone sees it; there is no module suffix spelled here.
    head = lastDot
  result.nameLen = head

  var i = head - 2
  while i > 0:
    if s[i] == '.' and s[i+1] in Digits: break
    dec i
  if i <= 0: return # not a symbol, or a reserved disambiguator (`_exit.sys.…`)

  result.wellFormed = true
  result.nameLen = i
  result.disambStart = i+1
  var d = i+1
  while d < head and s[d] in Digits:
    result.disamb = result.disamb * 10 + (ord(s[d]) - ord('0'))
    inc d
  if d < head and s[d] == '.':
    # whatever sits between the disambiguator and the module is the key a
    # generic instantiation is deduplicated by
    result.disambLen = d - result.disambStart
    result.disambIsNumeric = true
    result.dedupStart = d+1
    result.dedupLen = head - (d+1)
  else:
    # `p.0h107`: the disambiguator runs to the end of the head and is not a
    # number.
    result.disambLen = head - result.disambStart
    result.disambIsNumeric = d == head

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
  ## The module suffix of the symbol `s`, `""` when it is local -- the
  ## string-level answer, for a caller that has no `Pool` to ask (nifasm reads
  ## NIF symbols with no compiler around it). Code that HAS a pool asks
  ## `nifcore.symModule` / `sym(p, id).module` instead, so that after #2457 it
  ## reads a field rather than re-deriving one.
  ##
  ## Answered by `sliceSymbol`, byte-for-byte as the hand-rolled scanner this
  ## replaced -- including for arkham's `_exit.sys.sysvq0asl`, whose reserved
  ## non-numeric disambiguator is exactly what nifasm asks about.
  let sl = sliceSymbol(s)
  result = substr(s, sl.moduleStart, sl.moduleStart+sl.moduleLen-1)

proc genericTypeName*(key, modname: string): string =
  result = "`t.0.I" & key & "." & modname

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
    assert extractModule(s) == splitSymName(s).module, s
    if sl.wellFormed:
      assert substr(s, 0, sl.nameLen-1) == base, s
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

  for s in ["abc.12.Mod132a3bc", "abc.12", "a.b.c.23", "abc.12.Iabcdefghi.mod2",
            "tmp.14", "outer`env.0.mymod", "gen.12.Iaaaa`coro.0.mymod",
            "[]=", "foo.bar", "x.0", "_exit.sys.sysvq0asl", "write.c.sysvq0asl"]:
    agrees s

  assert extractModule("abc.12.Mod132a3bc") == "Mod132a3bc"
  assert extractModule("abc.12.Iabcdefghi.mod2") == "mod2"
  assert extractModule("abc.12") == ""
  assert extractModule("a.b.c.23") == ""
  # arkham mints these with a RESERVED non-numeric disambiguator; the module is
  # still the last component, and nifasm resolves the symbol by it.
  assert extractModule("_exit.sys.sysvq0asl") == "sysvq0asl"
  assert extractModule("write.c.sysvq0asl") == "sysvq0asl"
  # `foo.bar` is not a symbol at all; the last component answers anyway, as it
  # always has.
  assert extractModule("foo.bar") == "bar"
  # ...and a disambiguator the inliner minted is NOT a module, however
  # non-numeric it looks after its first character.
  assert extractModule("p.0h107") == ""
  assert extractModule("returnLabel.0h3") == ""
  assert extractModule("returnLabel.0h3.mymod") == "mymod"

  let minted = sliceSymbol("p.0h107")
  assert minted.wellFormed and minted.nameLen == 1
  assert not minted.disambIsNumeric
  assert substr("p.0h107", minted.disambStart, minted.disambStart+minted.disambLen-1) == "0h107"
  let plain = sliceSymbol("abc.12.Ikey.mod")
  assert plain.disambIsNumeric and plain.disamb == 12
  assert substr("abc.12.Ikey.mod", plain.disambStart, plain.disambStart+plain.disambLen-1) == "12"

  # A reserved disambiguator has no numeric anchor, so name and disambiguator
  # are not meaningful -- everything but the module is the name, which is what
  # `splitSymName` has always answered here (arkham/programs.nim relies on it).
  let sysSym = sliceSymbol("_exit.sys.sysvq0asl")
  assert not sysSym.wellFormed
  assert substr("_exit.sys.sysvq0asl", 0, sysSym.nameLen-1) ==
         splitSymName("_exit.sys.sysvq0asl").name
  assert extractModule("_exit.sys.sysvq0asl") ==
         splitSymName("_exit.sys.sysvq0asl").module

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
