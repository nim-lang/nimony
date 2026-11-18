#
#           Leng platform facts: `(defined "a" "b")`
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this distribution, for
#    details about the copyright.
#

## `(defined "linux" "bsd")` is a platform test that survives into Leng (see
## `doc/internals/leng-spec.md`, "Platform facts"): true when one of its
## string literals is a platform fact. Every backend resolves it right after
## parsing a module, before any other pass sees the tree:
##
## - each `defined` becomes `(true)` or `(false)`;
## - an `ite` whose condition mentions `defined` and so folds to a constant
##   (`true`/`false` under `and`/`or`/`not`/`par`) is replaced by its taken
##   branch, as a `(scope ...)`, so the dead branch never reaches code
##   generation and may refer to what does not exist on the target.
##
## Facts are plain strings: `--os:x` and `--cpu:x` imply the fact `x`, and
## `--plat:x` adds one. Labels are not validated against any list.
##
## Works on `nifcore` buffers whose tag pool is Leng-seeded
## (`nifcdecl.createLengTagPool`), as all backends' are.

when defined(nimony):
  {.feature: "assumeSync".} # the facts are set once, at startup

import nifcore, nifcdecl
import ".." / models / tags # TrueTagId, FalseTagId, ScopeTagId, StmtsTagId

var
  gResolvePlatformFacts*: bool   ## set by a backend: foreign decls get resolved too
  gPlatformFacts*: seq[string]   ## the facts `getDecl` resolves foreign decls with

proc setPlatformFacts*(facts: seq[string]) =
  ## Called by a backend (lengc, arkham, jorogumo) at startup. From then on
  ## `foreignmodules.getDecl` resolves every foreign declaration it loads, so
  ## an inlined foreign body never carries a `defined`.
  gPlatformFacts = facts
  gResolvePlatformFacts = true

proc containsDefined(n: Cursor): bool =
  if n.kind != TagLit: return false
  if n.exprKind == DefinedC: return true
  var x = sub(n)
  while x.hasMore:
    if containsDefined(x): return true
    skip x
  result = false

proc hasPlatformTests*(buf: var TokenBuf): bool =
  ## Cheap pre-check: most modules have no `defined` and need no copy.
  var n = beginRead(buf)
  result = false
  while n.hasMore:
    if containsDefined(n):
      result = true
      break
    skip n
  endRead(n)

proc isFact(n: Cursor; facts: openArray[string]): bool =
  ## `n` is a `(defined ...)`.
  var r = sub(n)
  while r.hasMore:
    if r.kind == StrLit:
      let s = r.strVal
      for f in facts:
        if f == s: return true
    skip r
  result = false

type Folded = enum Unknown, IsFalse, IsTrue

proc fold(n: Cursor; facts: openArray[string]): Folded =
  ## The value of a condition built from `defined`, `true`, `false`, `and`,
  ## `or`, `not` and `par`; `Unknown` for anything else.
  if n.kind != TagLit: return Unknown
  let k = n.exprKind
  if k == DefinedC:
    result = if isFact(n, facts): IsTrue else: IsFalse
  elif k == TrueC:
    result = IsTrue
  elif k == FalseC:
    result = IsFalse
  elif k == ParC:
    result = fold(sub(n), facts)
  elif k == NotC:
    let a = fold(sub(n), facts)
    result = if a == IsTrue: IsFalse elif a == IsFalse: IsTrue else: Unknown
  elif k == AndC or k == OrC:
    var x = sub(n)
    let a = fold(x, facts)
    skip x
    let b = fold(x, facts)
    if a == Unknown or b == Unknown: result = Unknown
    elif k == AndC: result = (if a == IsTrue and b == IsTrue: IsTrue else: IsFalse)
    else: result = (if a == IsTrue or b == IsTrue: IsTrue else: IsFalse)
  else:
    result = Unknown

proc copyResolved(dest: var TokenBuf; n: Cursor; facts: openArray[string])

proc copyBranch(dest: var TokenBuf; branch: Cursor; info: NifLineInfo;
                facts: openArray[string]) =
  ## The taken branch of a folded `ite`, as one statement.
  dest.openTag TagId(ord(ScopeTagId))
  dest.appendLineInfo info
  if branch.kind == DotToken:
    dest.openTag TagId(ord(StmtsTagId))
    dest.appendLineInfo info
    dest.closeTag()
  else:
    copyResolved dest, branch, facts
  dest.closeTag()

proc copyResolved(dest: var TokenBuf; n: Cursor; facts: openArray[string]) =
  if not containsDefined(n):
    dest.addSubtree n
    return
  if n.exprKind == DefinedC:
    dest.openTag TagId(ord(if isFact(n, facts): TrueTagId else: FalseTagId))
    dest.appendLineInfo rawLineInfo(n)
    dest.closeTag()
    return
  if n.stmtKind == IteS:
    # `(ite Cond Then Else)`; one with a `join` slot or a label is left to
    # the code generator, with its condition folded.
    var x = sub(n)
    let cond = x
    skip x
    let thenPart = x
    skip x
    let elsePart = x
    var children = 2
    if x.hasMore:
      inc children
      skip x
    if not x.hasMore and children == 3 and containsDefined(cond):
      let v = fold(cond, facts)
      if v == IsTrue:
        copyBranch dest, thenPart, rawLineInfo(n), facts
        return
      elif v == IsFalse:
        copyBranch dest, elsePart, rawLineInfo(n), facts
        return
  dest.openTag n.cursorTagId
  dest.appendLineInfo rawLineInfo(n)
  var x = sub(n)
  while x.hasMore:
    copyResolved dest, x, facts
    skip x
  dest.closeTag()

proc resolvePlatformFacts*(buf: var TokenBuf; facts: openArray[string]) =
  ## Folds every `(defined ...)` in `buf` and drops the dead branches of the
  ## `ite`s that test them.
  if not hasPlatformTests(buf): return
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len, sharedPool = n.pool, sharedTags = n.tags)
  while n.hasMore:
    copyResolved dest, n, facts
    skip n
  endRead(n)
  buf = ensureMove dest
