#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Path handling and `exec` like features as `sem.nim` needs it.

import std / [tables, sets, os, syncio, formatfloat, assertions]
include nifprelude
import nimony_model, symtabs, builtintypes, decls, symparser, asthelpers,
  programs, sigmatch, magics, reporters, nifconfig, nifindexes,
  semdata

import ".." / gear2 / modnames

proc stdFile(f: string): string =
  getAppDir() / "lib" / f

proc resolveFile*(c: SemContext; origin: string; toResolve: string): string =
  let nimFile = toResolve.addFileExt(".nim")
  #if toResolve.startsWith("std/") or toResolve.startsWith("ext/"):
  #  result = stdFile nimFile
  if toResolve.isAbsolute:
    result = nimFile
  else:
    result = splitFile(origin).dir / nimFile
    var i = 0
    while not fileExists(result) and i < c.g.config.paths.len:
      result = c.g.config.paths[i] / nimFile
      inc i

type ImportedFilename* = object
  path*: string
  name*: string

proc filenameVal*(n: var Cursor; res: var seq[ImportedFilename]; hasError: var bool; allowAs = false) =
  case n.kind
  of StringLit, Ident:
    let s = pool.strings[n.litId]
    # maybe call splitFile and get filename without extension for name:
    res.add ImportedFilename(path: s, name: s)
    inc n
  of Symbol:
    var s = pool.syms[n.symId]
    extractBasename s
    res.add ImportedFilename(path: s, name: s)
    inc n
  of ParLe:
    case exprKind(n)
    of OchoiceX, CchoiceX:
      inc n
      if n.kind != ParRi:
        filenameVal(n, res, hasError)
        while n.kind != ParRi: skip n
        inc n
      else:
        hasError = true
        inc n
    of QuotedX:
      let s = pool.strings[unquote(n)]
      res.add ImportedFilename(path: s, name: s)
    of CallX, InfixX:
      var x = n
      skip n # ensure we skipped it completely
      inc x
      var op = ""
      let opId = getIdent(x)
      if opId == StrId(0):
        hasError = true
      else:
        op = pool.strings[opId]
      if hasError:
        discard
      elif op == "as":
        if not allowAs:
          hasError = true
          return
        if x.kind == ParRi:
          hasError = true
          return
        var rhs = x
        skip rhs # skip lhs
        if rhs.kind == ParRi:
          hasError = true
          return
        let aliasId = getIdent(rhs)
        if aliasId == StrId(0):
          hasError = true
        else:
          let alias = pool.strings[aliasId]
          var prefix: seq[ImportedFilename] = @[]
          filenameVal(x, prefix, hasError, allowAs = false)
          if x.kind != ParRi: hasError = true
          for pre in mitems(prefix):
            if pre.path != "":
              res.add ImportedFilename(path: pre.path, name: alias)
          if prefix.len == 0:
            hasError = true
      else: # any operator, could restrict to slash-like
        var prefix: seq[ImportedFilename] = @[]
        filenameVal(x, prefix, hasError, allowAs = false)
        var suffix: seq[ImportedFilename] = @[]
        filenameVal(x, suffix, hasError, allowAs = allowAs)
        if x.kind != ParRi: hasError = true
        for pre in mitems(prefix):
          for suf in mitems(suffix):
            if pre.path != "" and suf.path != "":
              res.add ImportedFilename(path: pre.path & op & suf.path, name: suf.name)
            else:
              hasError = true
        if prefix.len == 0 or suffix.len == 0:
          hasError = true
    of PrefixX:
      var x = n
      skip n # ensure we skipped it completely
      inc x
      var op = ""
      let opId = getIdent(x)
      if opId == StrId(0):
        hasError = true
      else:
        op = pool.strings[opId]
      if hasError:
        discard
      else: # any operator, could restrict to slash-like
        var suffix: seq[ImportedFilename] = @[]
        filenameVal(x, suffix, hasError, allowAs = allowAs)
        if x.kind != ParRi: hasError = true
        for suf in mitems(suffix):
          if suf.path != "":
            res.add ImportedFilename(path: op & suf.path, name: suf.name)
          else:
            hasError = true
        if suffix.len == 0:
          hasError = true
    of ParX, AconstrX:
      inc n
      if n.kind != ParRi:
        while n.kind != ParRi:
          filenameVal(n, res, hasError, allowAs)
        inc n
      else:
        hasError = true
        inc n
    of TupleConstrX:
      inc n
      skip n # skip type
      if n.kind != ParRi:
        while n.kind != ParRi:
          filenameVal(n, res, hasError, allowAs)
        inc n
      else:
        hasError = true
        inc n
    else:
      skip n
      hasError = true
  else:
    skip n
    hasError = true

# ------------------ include/import handling ------------------------

proc findTool*(name: string): string =
  let exe = name.addFileExt(ExeExt)
  result = getAppDir() / exe

proc exec*(cmd: string) =
  if execShellCmd(cmd) != 0: quit("FAILURE: " & cmd)

proc parseFile*(nimFile: string; paths: openArray[string]): TokenBuf =
  let nifler = findTool("nifler")
  let name = moduleSuffix(nimFile, paths)
  let src = "nifcache" / name & ".1.nif"
  exec quoteShell(nifler) & " --portablePaths p " & quoteShell(nimFile) & " " &
    quoteShell(src)

  var stream = nifstreams.open(src)
  try:
    discard processDirectives(stream.r)
    result = fromStream(stream)
  finally:
    nifstreams.close(stream)

proc getFile*(c: var SemContext; info: PackedLineInfo): string =
  let (fid, _, _) = unpack(pool.man, info)
  result = pool.files[fid]

proc selfExec*(c: var SemContext; file: string) =
  exec os.getAppFilename() & c.commandLineArgs & " m " & quoteShell(file)
