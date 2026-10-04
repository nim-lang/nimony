#
#
#           Nif library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The interface of a Leng module (`M.c.nif`, `M.oc.nif`): the part of it
## that the codegen nodes of OTHER modules read. `lengc` and `arkham` resolve
## a foreign symbol by loading its declaration out of the owning module's
## file: types, globals, constants and string literals, proc signatures, and
## the BODIES of `.inline` procs, which `lengc` emits into every translation
## unit that calls them. A non-inline proc body is never read from outside.
##
## The interface file `M.c.idx.nif` (`M.oc.idx.nif`) records a digest of
## exactly that part and is rewritten only when the digest changes, so a
## changed proc body re-runs the codegen of its own module and of nothing
## else. See doc/internals/ic.md.

import std / [syncio, assertions, formatfloat]
import nifcore, nifbuilder
from vfs import OnlyIfChanged

when defined(nimony):
  import std / sha1
else:
  {.push warning[Deprecated]: off.}
  import std / sha1
  {.pop.}

proc lengInterfaceFile*(lengFile: string): string =
  ## `M.c.nif` -> `M.c.idx.nif`, `M.oc.nif` -> `M.oc.idx.nif`.
  const ext = ".nif"
  assert lengFile.len > ext.len and
    lengFile.substr(lengFile.len - ext.len) == ext, "not a .nif file: " & lengFile
  result = lengFile.substr(0, lengFile.len - ext.len - 1) & ".idx.nif"

proc hashTree(s: var Sha1State; n: var Cursor; foundInline: var bool) =
  ## Hashes the tree at `n` and advances past it. Line information is not
  ## part of the digest, as for `.s.idx.nif`.
  case n.kind
  of TagLit:
    let tag = n.tags.tagName(n.cursorTagId)
    if tag == "inline": foundInline = true
    s.update "("
    s.update tag
    n.into:
      while n.hasMore:
        hashTree(s, n, foundInline)
    s.update ")"
  of Symbol:
    s.update " "
    s.update n.symName
    inc n
  of SymbolDef:
    s.update " :"
    s.update n.symName
    inc n
  of Ident:
    s.update " "
    s.update n.strVal
    inc n
  of StrLit:
    s.update " \""
    s.update n.strVal
    inc n
  of IntLit:
    s.update " "
    s.update $n.intVal
    inc n
  of UIntLit:
    s.update " "
    s.update $n.uintVal
    inc n
  of FloatLit:
    s.update " "
    s.update $n.floatVal
    inc n
  of CharLit:
    s.update " '"
    s.update $ord(n.charLit)
    inc n
  of DotToken:
    s.update " ."
    inc n
  else:
    inc n

proc lengInterfaceDigest*(buf: var TokenBuf): string =
  ## Digest of everything in the module except the bodies of non-inline procs.
  var s = newSha1State()
  var n = beginRead(buf)
  if n.kind == TagLit:
    s.update "("
    s.update n.tags.tagName(n.cursorTagId)
    n.into:
      while n.hasMore:
        if n.kind == TagLit and n.tags.tagName(n.cursorTagId) == "proc":
          # (proc SymbolDef Params Type ProcPragmas Body)
          s.update "(proc"
          var isInline = false
          n.into:
            var i = 0
            while i < 4 and n.hasMore:
              hashTree(s, n, isInline)
              inc i
            while n.hasMore:
              if isInline:
                var ignored = false
                hashTree(s, n, ignored)
              else:
                skip n
          s.update ")"
        else:
          var ignored = false
          hashTree(s, n, ignored)
    s.update ")"
  endRead(n)
  result = $SecureHash(s.finalize())

proc writeLengInterface*(buf: var TokenBuf; lengFile: string) =
  ## Writes `lengFile`'s interface file next to it, unless it already records
  ## the same digest: then its modification time stays, and so do the codegen
  ## nodes of the other modules.
  var b = nifbuilder.open(lengInterfaceFile(lengFile), writeMode = OnlyIfChanged)
  b.addHeader()
  b.withTree "interface":
    b.addStrLit lengInterfaceDigest(buf)
  b.close()
