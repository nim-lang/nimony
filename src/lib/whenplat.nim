#
#           Leng `whenplat` resolution
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution, for
#    details about the copyright.
#

## `(whenplat (of (ranges "linux" "bsd") S)+ (else S)?)` is platform-specific
## code that survives into Leng (see `doc/internals/leng-spec.md`, "Platform
## facts"). Every backend resolves it right after parsing a module, before any
## other pass sees the tree: the first branch with a label that is one of the
## platform facts wins, else the `else` branch; no match and no `else` is a
## compile-time error. The taken branch becomes a `(scope ...)`, so it still
## is exactly one statement and its locals keep their scope.
##
## Facts are plain strings: `--os:x` and `--cpu:x` imply the fact `x`, and
## `--plat:x` adds one. Labels are not validated against any list.
##
## Works on `nifcore` buffers whose tag pool is Leng-seeded
## (`nifcdecl.createLengTagPool`), as all backends' are.

when defined(nimony):
  {.feature: "assumeSync".} # the facts are set once, at startup

import nifcore, nifcdecl
import ".." / models / tags # ScopeTagId

var
  gResolveWhenplat*: bool        ## set by a backend: foreign decls get resolved too
  gPlatformFacts*: seq[string]   ## the facts `getDecl` resolves foreign decls with

proc setPlatformFacts*(facts: seq[string]) =
  ## Called by a backend (lengc, `shoggoth opt`, arkham) at startup. From then
  ## on `foreignmodules.getDecl` resolves the `whenplat`s of every foreign
  ## declaration it loads, so an inlined foreign body never carries one.
  gPlatformFacts = facts
  gResolveWhenplat = true

proc containsWhenplat(n: Cursor): bool =
  if n.kind != TagLit: return false
  if n.stmtKind == WhenplatS: return true
  var x = sub(n)
  while x.hasMore:
    if containsWhenplat(x): return true
    skip x
  result = false

proc hasWhenplat*(buf: var TokenBuf): bool =
  ## Cheap pre-check: most modules have no `whenplat` and need no copy.
  var n = beginRead(buf)
  result = false
  while n.hasMore:
    if containsWhenplat(n):
      result = true
      break
    skip n
  endRead(n)

proc labelsMatch(ranges: Cursor; facts: openArray[string]): bool =
  var r = sub(ranges)
  while r.hasMore:
    if r.kind == StrLit:
      let s = r.strVal
      for f in facts:
        if f == s: return true
    skip r
  result = false

proc where(n: Cursor): string =
  let li = rawLineInfo(n)
  let file = lineInfoFile(n)
  if file.len > 0: result = file & "(" & $li.line & ", " & $li.col & "): "
  else: result = ""

proc copyResolved(dest: var TokenBuf; n: Cursor; facts: openArray[string];
                  err: var string) =
  if err.len > 0: return
  if not containsWhenplat(n):
    dest.addSubtree n
    return
  if n.stmtKind == WhenplatS:
    var b = sub(n)
    var found = false
    var chosen = default(Cursor)
    while b.hasMore and not found:
      let k = b.substructureKind
      if k == OfU:
        var x = sub(b)
        if labelsMatch(x, facts):
          skip x # the labels
          chosen = x
          found = true
      elif k == ElseU:
        chosen = sub(b)
        found = true
      else:
        err = where(b) & "`whenplat` expects `of` or `else` branches"
        return
      skip b
    if not found:
      var list = ""
      for f in facts:
        if list.len > 0: list.add ", "
        list.add f
      err = where(n) & "no `whenplat` branch matches the platform facts [" &
        list & "] and there is no `else` branch"
      return
    dest.openTag TagId(ord(ScopeTagId))
    dest.appendLineInfo rawLineInfo(n)
    copyResolved dest, chosen, facts, err
    dest.closeTag()
  else:
    dest.openTag n.cursorTagId
    dest.appendLineInfo rawLineInfo(n)
    var x = sub(n)
    while x.hasMore:
      copyResolved dest, x, facts, err
      skip x
    dest.closeTag()

proc resolveWhenplats*(buf: var TokenBuf; facts: openArray[string]): string =
  ## Replaces every `whenplat` in `buf` by its taken branch. Returns "" on
  ## success, else the error message (and `buf` is left unchanged).
  result = ""
  if not hasWhenplat(buf): return
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len, sharedPool = n.pool, sharedTags = n.tags)
  while n.hasMore:
    copyResolved dest, n, facts, result
    skip n
  endRead(n)
  if result.len == 0:
    buf = ensureMove dest
