#       Nimony
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Expansion provenance encoded in a line-info filename.
##
## Code produced by expanding a template does not get its own wrapper node.
## Instead the tokens carry a *forged* filename that records what they came
## from, so a debug backend can emit them as DWARF inlined frames:
##
##   __crucial\0setElem.0.foo\1foo.nim\116\0[]=.0.system\1system.nim\134\0system.nim
##   ^prefix    ^-------- outermost -------^ ^--------- innermost --------^ ^real file
##
## The chain runs outermost-first, so its length is the inlining depth. Each
## entry is `<sym>\1<declfile>\1<declline>`: the symbol names the expanded
## routine, and the declaration site is carried because it cannot be recovered
## later - a template declaration does not survive into the backend, and the
## expanded code's own line info points at wherever the body came from, not at
## the template. Everything after the last NUL is the real filename, which is
## what a consumer that does not care about frames should use.
##
## A filename cannot otherwise contain a NUL or a `\1`, which is what makes the
## encoding unambiguous - note that `|` would not do, since Nim lets an operator
## be named `|`. `nifbuilder.needsEscape` covers `c < ' '`, so both control
## characters survive text NIF as `\00` / `\01`; `bif` writes filenames
## length-prefixed, so binary is fine.
##
## Deliberately free of any NIF dependency: it is plain string handling, so the
## front end, the C backend and the LLVM backend can all reach it without
## pulling in a cursor API.

const
  CrucialPrefix* = "__crucial\0"
    ## Marks a forged filename that carries template-expansion provenance.
  CrucialFieldSep* = '\1'
    ## Separates `<sym>`, `<declfile>` and `<declline>` inside one chain entry.

type
  CrucialOrigin* = object ## One level of an expansion chain.
    sym*: string        ## the expanded routine, mangled (`setElem.0.foo`)
    declFile*: string   ## where it was declared; "" when unknown
    declLine*: int32    ## its declaration line; 0 when unknown

proc isCrucialFile*(fname: string): bool {.noSideEffect.} =
  ## True when `fname` carries expansion provenance rather than being a plain
  ## source path. Cheap enough to call per token: the `\0` at the end of the
  ## prefix is checked first, and a real path never has one.
  if fname.len <= CrucialPrefix.len: return false
  if fname[CrucialPrefix.len - 1] != '\0': return false
  for i in 0 ..< CrucialPrefix.len:
    if fname[i] != CrucialPrefix[i]: return false
  result = true

proc addCrucialInfo*(dest: var string; sym, declFile: string;
                     declLine: int32) {.noSideEffect.} =
  ## Append one expansion entry to a forged filename under construction.
  ## Entries are written outermost first; what follows the last one is the real
  ## filename. `dest` starts as `CrucialPrefix`, so building a name is
  ##
  ##   var f = CrucialPrefix
  ##   f.addCrucialInfo(sym, declFile, declLine)
  ##   f.add crucialTail(oldName)
  ##
  ## which prepends onto whatever chain `oldName` already had without parsing
  ## it back out.
  dest.add sym
  dest.add CrucialFieldSep
  dest.add declFile
  dest.add CrucialFieldSep
  dest.add $declLine
  dest.add '\0'

proc crucialTail*(fname: string): string {.noSideEffect.} =
  ## Everything of `fname` that follows `CrucialPrefix`: its existing entries
  ## plus the real filename, in the exact form they must keep. For a plain
  ## filename that is the name itself, so appending this after some entries
  ## works whether or not `fname` was already forged.
  if isCrucialFile(fname): fname.substr(CrucialPrefix.len) else: fname

proc realFile*(fname: string): string {.noSideEffect.} =
  ## The actual source file, with any expansion provenance stripped. Returns
  ## `fname` unchanged when it carries none, so every consumer can call it.
  if not isCrucialFile(fname): return fname
  var last = -1
  for i in 0 ..< fname.len:
    if fname[i] == '\0': last = i
  if last < 0: fname else: fname.substr(last + 1)

proc parseCrucialOrigin(entry: string): CrucialOrigin {.noSideEffect.} =
  ## Split one `<sym>\1<declfile>\1<declline>` entry. Tolerates a bare symbol
  ## with no declaration site, so a partially-known chain still names its
  ## frames instead of being discarded.
  result = CrucialOrigin(sym: entry, declFile: "", declLine: 0'i32)
  var first = -1
  var second = -1
  for i in 0 ..< entry.len:
    if entry[i] == CrucialFieldSep:
      if first < 0: first = i
      elif second < 0: second = i
  if first < 0: return
  result.sym = entry.substr(0, first - 1)
  if second < 0:
    result.declFile = entry.substr(first + 1)
    return
  result.declFile = entry.substr(first + 1, second - 1)
  var line = 0'i32
  for i in second + 1 ..< entry.len:
    let ch = entry[i]
    if ch < '0' or ch > '9': return
    line = line * 10'i32 + int32(ord(ch) - ord('0'))
  result.declLine = line

iterator crucialOrigins*(fname: string): CrucialOrigin {.noSideEffect.} =
  ## The expanded routines `fname` came from, outermost first. Yields nothing
  ## for a plain filename.
  if isCrucialFile(fname):
    var start = CrucialPrefix.len
    var i = start
    while i < fname.len:
      if fname[i] == '\0':
        yield parseCrucialOrigin(fname.substr(start, i - 1))
        start = i + 1
      inc i
    # the tail after the last NUL is the real filename, not an origin
