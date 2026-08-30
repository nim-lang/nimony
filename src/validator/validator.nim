#
#
#           Nimony Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Validates compiler-pass and plugin source code for structural correctness:
## that the NIF a routine builds conforms to the grammar in `doc/tags.md`, and
## that its traversal of the input is accounted for -- every `var Cursor`
## parameter consumed, every branch's advance matched by what it emits, every
## cursor-bounded loop provably advancing.
##
## The work is done over the *semchecked* module (`semfacts.nim`,
## `semvalidator.nim`), so types, callees and variables are resolved symbols
## rather than spellings. This module is only the driver: it finds the `.s.nif`
## sem produced for the file it was asked about, and reports what comes back.
##
## Usage: validator [--strict] [--nimcache:DIR] [--dump] [--dumptrees]
##                  <passfile.nim|module.s.nif> [tags.md]
##
## `--strict` adds the checks that only make sense for compiler-pass source
## (exhaustive `case` over a tag kind, bare `skip`/`inc` on a cursor). Plugins
## are validated without it: `else: takeTree n` pass-through is idiomatic for
## them.
##
## `--nimcache:DIR` names the cache a `nimony check` wrote the module into; the
## file may also be named directly as a `.s.nif`. `--dump` prints what the
## front end made of the module and `--dumptrees` every tree it saw built --
## the first things to look at when a check fires where it should not.

import std / [strutils, os, terminal, syncio]
include ".." / lib / nifprelude
import ".." / lib / nifcoreparse
import ".." / models / [tags, nimony_tags]
import tags_grammar
import semvalidator

proc includerOf(nimFile, cacheDir: string): string =
  ## An `include`d file is semchecked as part of whoever includes it and has no
  ## `.s.nif` of its own, so validating one means reading the includer's module
  ## and keeping the procs that came from this file — which is exactly what
  ## passing the source path alongside the NIF already does.
  ##
  ## The includes are recorded in each module's `.p.deps.nif` as
  ## `(include <basename>)`, which is all nifler knows; two files of the same
  ## basename in different directories would be indistinguishable here.
  result = ""
  let want = splitFile(nimFile).name
  for x in walkDir(cacheDir, relative = true):
    if x.kind != pcFile or not x.path.endsWith(".p.deps.nif"): continue
    var buf = nifcoreparse.parseFromFile(cacheDir / x.path, sharedPool = pool,
                                         sharedTags = globalTags)
    var n = beginRead(buf)
    if not n.isTagLit: continue
    var found = false
    n.linearScan:
      if globalTags.tags[n.cursorTagId] == "include":
        var c = childCursor(n)
        if c.hasMore and (c.kind == Ident or c.kind == StrLit) and c.strVal == want:
          found = true
    if found:
      let suffix = x.path.substr(0, x.path.len - ".p.deps.nif".len - 1)
      let candidate = cacheDir / suffix & ".s.nif"
      if fileExists(candidate): return candidate

proc semNifForSource(nimFile, cacheDir: string): string =
  ## Map a `.nim` source to the `.s.nif` sem produced for it, using the
  ## `<root>.build.nif` files nifmake leaves in the cache. That mapping is
  ## authoritative: `moduleSuffix` hashes a path made relative to the compile's
  ## own search paths, so recomputing it here would mean replicating the exact
  ## command line the module was built with.
  result = ""
  let want = absolutePath(nimFile).replace('\\', '/')
  for x in walkDir(cacheDir, relative = true):
    if x.kind != pcFile or not x.path.endsWith(".build.nif"): continue
    var buf = nifcoreparse.parseFromFile(cacheDir / x.path, sharedPool = pool,
                                         sharedTags = globalTags)
    var n = beginRead(buf)
    if not n.isTagLit: continue
    n.linearScan:
      if globalTags.tags[n.cursorTagId] == "do":
        var c = childCursor(n)
        var input = ""
        var output = ""
        while c.hasMore:
          if c.isTagLit:
            let tag = globalTags.tags[c.cursorTagId]
            if tag in ["input", "output"]:
              var v = childCursor(c)
              if v.hasMore and v.kind == StrLit:
                if tag == "input": input = v.strVal
                else: output = v.strVal
          skip c
        if input.len > 0 and output.endsWith(".p.nif") and
            absolutePath(input).replace('\\', '/') == want:
          result = cacheDir / extractFilename(output).changeFileExt("").changeFileExt(".s.nif")
          return

proc main() =
  if paramCount() < 1:
    quit "Usage: validator [--strict] [--nimcache:DIR] [--dump] [--dumptrees] " &
         "<passfile.nim|module.s.nif> [tags.md]"

  var strict = false
  var cacheDir = ""
  var dump = false
  var dumpTrees = false
  var positional: seq[string] = @[]
  for i in 1..paramCount():
    let a = paramStr(i)
    if a == "--strict": strict = true
    elif a == "--dump": dump = true
    elif a == "--dumptrees": dumpTrees = true
    elif a.startsWith("--nimcache:"): cacheDir = a.substr("--nimcache:".len)
    else: positional.add a
  if positional.len < 1:
    quit "Usage: validator [--strict] [--nimcache:DIR] [--dump] [--dumptrees] " &
         "<passfile.nim|module.s.nif> [tags.md]"

  let tagsFile = if positional.len >= 2: positional[1]
                 else:
                   # Candidates, in order: appDir/../doc/tags.md (bin in project root),
                   # appDir/../../doc/tags.md (bin nested one deeper), cwd/doc/tags.md.
                   let appDir = getAppDir()
                   var candidate = appDir / ".." / "doc" / "tags.md"
                   if not fileExists(candidate):
                     candidate = appDir / ".." / ".." / "doc" / "tags.md"
                   if not fileExists(candidate):
                     candidate = "doc/tags.md"
                   candidate
  if not fileExists(tagsFile):
    quit "Cannot find tags.md at: " & tagsFile

  # Either the caller names the `.s.nif` outright, or it names the source and
  # the cache sem wrote it into.
  var semNif = ""
  var source = ""
  if positional[0].endsWith(".s.nif"):
    semNif = positional[0]
  else:
    source = positional[0]
    if not fileExists(source):
      quit "Cannot find source file: " & source
    if cacheDir.len == 0:
      quit "--nimcache:DIR is required to validate " & source &
           " (it names the cache `nimony check` wrote the module into)"
    semNif = semNifForSource(source, cacheDir)
    if semNif.len > 0 and not fileExists(semNif):
      semNif = includerOf(source, cacheDir)
    if semNif.len == 0:
      quit "no semchecked NIF for " & source & " in " & cacheDir &
           " (was it built with `nimony check --nimcache:" & cacheDir & "`?)"
  if not fileExists(semNif):
    quit "Cannot find semchecked NIF: " & semNif

  let errors = validateSemModule(semNif, source, parseTagsMd(tagsFile), strict,
                                 not terminal.isatty(stdout), dump, dumpTrees)
  if errors > 0: quit 1

main()
