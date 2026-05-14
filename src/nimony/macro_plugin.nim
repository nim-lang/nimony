#       Nimony Compiler
# (c) Copyright 2024-2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Macro plugin support: build the plugin module as NIF directly, hand it to
## `nimony s` (which picks up `.p.nif` input — same machinery `executeExpr`
## uses for CTFE), and exec the resulting binary at every call site.

import std/[syncio, os, osproc, tables, hashes, assertions]

import ".." / lib / [nifstreams, nifcursors, lineinfos, bitabs, nifindexes, symparser]
import ".." / models / [tags]
import nimony_model, decls, programs

type
  MacroPlugin* = object
    exePath*: string

proc hash(s: SymId): Hash {.borrow.}

proc cleanSymbolName(s: string): string =
  ## Extract the base name from a fully-qualified symbol (strip `.0.suffix`).
  let dotPos = s.find('.')
  if dotPos >= 0:
    result = substr(s, 0, dotPos - 1)
  else:
    result = s

# ----------------------------------------------------------------------------
# NIF builder helpers (no string rendering involved)
# ----------------------------------------------------------------------------

proc spliceBodyWithoutResult(dest: var TokenBuf; body: Cursor) =
  ## Copy `body` (a `(stmts ...)` subtree) into `dest`, but drop a leading
  ## `(result :result.X . . <type> .)` declaration if present. The wrapping
  ## proc owns the implicit `result`; the body's local declaration (which for
  ## a macro returning `untyped` would otherwise carry type `untyped`) is
  ## redundant and prevents sem from giving `result` the wrapping proc's
  ## return type.
  var n = body
  assert n.stmtKind == StmtsS, "macro body should be a stmts block"
  dest.add n.load
  inc n
  if n.kind == ParLe and n.stmtKind == ResultS:
    # Skip the leading result declaration.
    var nested = 1
    inc n
    while nested > 0:
      if n.kind == ParLe: inc nested
      elif n.kind == ParRi: dec nested
      inc n
  while n.kind != ParRi:
    dest.addSubtree n
    skip n
  dest.add n.load  # the closing ParRi of stmts

proc rewriteSymsToIdents(buf: var TokenBuf) =
  ## Convert every Symbol / SymbolDef in `buf` to an Ident bearing the symbol's
  ## base name, and unwrap `(ochoice …)` / `(cchoice …)` nodes the same way.
  ## After this pass, the buffer reads like nifler output — sem will resolve
  ## every name against the plugin module's own scope.
  var newBuf = createTokenBuf(buf.len)
  var n = beginRead(buf)
  var nested = 0
  while true:
    case n.kind
    of Symbol, SymbolDef:
      var name = pool.syms[n.symId]
      extractBasename name
      newBuf.add identToken(pool.strings.getOrIncl(name), n.info)
      inc n
    of ParLe:
      let ek = n.exprKind
      if ek == OchoiceX or ek == CchoiceX:
        inc n
        if n.kind == Symbol:
          var name = pool.syms[n.symId]
          extractBasename name
          newBuf.add identToken(pool.strings.getOrIncl(name), n.info)
          while n.kind != ParRi: skip n
          inc n  # consume ParRi
        else:
          newBuf.add n.load
          inc nested
          inc n
      else:
        newBuf.add n.load
        inc nested
        inc n
    of ParRi:
      newBuf.add n.load
      dec nested
      if nested == 0: break
      inc n
    of EofToken:
      break
    else:
      newBuf.add n.load
      inc n
  endRead(buf)
  buf = ensureMove newBuf

# ----------------------------------------------------------------------------
# Plugin module generator
# ----------------------------------------------------------------------------

proc emitImportStdMacros(dest: var TokenBuf; info: PackedLineInfo) =
  ## Emit `(import (infix / std (bracket syncio macros)))` — the same shape
  ## nifler emits for `import std/[syncio, macros]`.
  dest.copyInto(pool.tags.getOrIncl("import"), info):
    dest.copyInto(pool.tags.getOrIncl("infix"), info):
      dest.addIdent "/", info
      dest.addIdent "std", info
      dest.copyInto(pool.tags.getOrIncl("bracket"), info):
        dest.addIdent "syncio", info
        dest.addIdent "macros", info

proc emitNimNodeRetType(dest: var TokenBuf; info: PackedLineInfo) =
  ## Pre-sem the return type is just the ident `NimNode`. Sem resolves it
  ## against `lib/std/macros.nim`'s `NimNode*` type.
  dest.addIdent "NimNode", info

proc emitImplProc(dest: var TokenBuf; implName: string; macroDecl: Cursor;
                  info: PackedLineInfo) =
  ## Emit `(proc <implName> . . . (params <copied>) NimNode . . <body>)`.
  let r = asRoutine(macroDecl, SkipInclBody)
  dest.copyInto(pool.tags.getOrIncl("proc"), info):
    dest.addIdent implName, info
    dest.addEmpty3 info                       # exported, pattern, typevars
    # params: copy verbatim (the user's signature) — sem will resolve param
    # type idents/syms freshly in the plugin module.
    if r.params.kind == DotToken:
      dest.copyInto(pool.tags.getOrIncl("params"), info):
        discard
    else:
      dest.addSubtree r.params
    emitNimNodeRetType(dest, info)
    dest.addEmpty2 info                       # pragmas, effects
    spliceBodyWithoutResult(dest, r.body)

proc emitMainProc(dest: var TokenBuf; implName: string; paramCount: int;
                  info: PackedLineInfo) =
  ## Emit:
  ##   proc main =
  ##     let input = loadInput()
  ##     let arg0 = input[0]; let arg1 = input[1]; ...
  ##     let output = <implName>(arg0, arg1, ...)
  ##     saveOutput(output)
  let procTag = pool.tags.getOrIncl("proc")
  let letTag = pool.tags.getOrIncl("let")
  let callTag = pool.tags.getOrIncl("call")
  let stmtsTag = pool.tags.getOrIncl("stmts")
  let paramsTag = pool.tags.getOrIncl("params")
  let bracketExprTag = pool.tags.getOrIncl("at")

  dest.copyInto(procTag, info):
    dest.addIdent "main", info
    dest.addEmpty3 info                       # exported, pattern, typevars
    dest.copyInto(paramsTag, info):
      discard
    dest.addEmpty info                        # return type (void)
    dest.addEmpty2 info                       # pragmas, effects
    dest.copyInto(stmtsTag, info):
      # let input = loadInput()
      dest.copyInto(letTag, info):
        dest.addIdent "input", info
        dest.addEmpty3 info                   # exported, pragmas, type
        dest.copyInto(callTag, info):
          dest.addIdent "loadInput", info

      # let argN = input[N]
      for i in 0 ..< paramCount:
        dest.copyInto(letTag, info):
          dest.addIdent "arg" & $i, info
          dest.addEmpty3 info
          dest.copyInto(bracketExprTag, info):
            dest.addIdent "input", info
            dest.addIntLit i, info

      # let output = implName(arg0, arg1, ...)
      dest.copyInto(letTag, info):
        dest.addIdent "output", info
        dest.addEmpty3 info
        dest.copyInto(callTag, info):
          dest.addIdent implName, info
          for i in 0 ..< paramCount:
            dest.addIdent "arg" & $i, info

      # saveOutput(output)
      dest.copyInto(callTag, info):
        dest.addIdent "saveOutput", info
        dest.addIdent "output", info

proc countParams(macroDecl: Cursor): int =
  result = 0
  let r = asRoutine(macroDecl, SkipInclBody)
  if r.params.kind == DotToken or r.params.substructureKind != ParamsU:
    return 0
  var p = r.params
  inc p
  while p.kind != ParRi:
    if p.substructureKind == ParamU:
      inc result
    skip p

proc buildPluginNif*(macroDecl: Cursor; macroSym: SymId;
                     info: PackedLineInfo): TokenBuf =
  ## Build a `.p.nif`-shaped TokenBuf for a macro plugin module.
  ##
  ## Shape:
  ##   (stmts
  ##     (import (infix / std (bracket syncio macros)))
  ##     (proc <macroName>Impl . . . (params …) NimNode . . <body>)
  ##     (proc main . . . (params) . . . (stmts …))
  ##     (call main))
  ##
  ## Symbols from the post-sem macro decl are rewritten to idents at the end
  ## so the plugin module re-runs through sem with its own scope.
  result = createTokenBuf(128)
  let macroName = cleanSymbolName(pool.syms[macroSym])
  let implName = macroName & "Impl"
  let paramCount = countParams(macroDecl)

  result.copyInto(pool.tags.getOrIncl("stmts"), info):
    emitImportStdMacros(result, info)
    emitImplProc(result, implName, macroDecl, info)
    emitMainProc(result, implName, paramCount, info)
    # call main()
    result.copyInto(pool.tags.getOrIncl("call"), info):
      result.addIdent "main", info

  rewriteSymsToIdents(result)

# ----------------------------------------------------------------------------
# Driver: write the NIF, build with Nimony, exec at call sites
# ----------------------------------------------------------------------------

proc getMacroPluginPath*(nifcachePath: string; macroSym: SymId): string =
  let symName = pool.syms[macroSym]
  var cleanName = ""
  for ch in symName:
    if ch in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
      cleanName.add ch
    else:
      cleanName.add '_'
  result = nifcachePath / "macro_" & cleanName
  when defined(windows):
    result.add ".exe"

proc compileMacroPlugin*(nifcachePath: string; macroDecl: Cursor; macroSym: SymId;
                         info: lineinfos.PackedLineInfo): string =
  ## Build the plugin module straight from NIF (no Nim text round-trip), write
  ## it as a `.p.nif`, and have Nimony compile it through `s` (the NIF-input
  ## entry point — same one CTFE uses in `semos.runEval`).
  let exePath = getMacroPluginPath(nifcachePath, macroSym)
  let pluginBaseName = "macro_" & $macroSym.int
  let progfile = nifcachePath / pluginBaseName.addFileExt(".p.nif")

  var buf = buildPluginNif(macroDecl, macroSym, info)
  try:
    writeFileAndIndex(progfile, buf)
  except:
    echo "Macro plugin: failed to write ", progfile
    return ""

  # `nimony s` opens `<progfile>.p.deps.nif` unconditionally — write an empty
  # `(stmts)` (the plugin module has no external NIF dependencies of its own,
  # only stdlib imports which Nimony discovers via its normal search path).
  let depsFile = nifcachePath / pluginBaseName & ".p.deps.nif"
  var deps = createTokenBuf(4)
  deps.addParLe StmtsS, info
  deps.addParRi()
  try:
    writeFile(deps, depsFile)
  except:
    echo "Macro plugin: failed to write ", depsFile
    return ""

  let nimonyExe = getAppDir() / "nimony"
  let srcLibPath = getAppDir().parentDir() / "src" / "lib"
  let cmd = quoteShell(nimonyExe) &
            " --path:" & quoteShell(srcLibPath) &
            " -o:" & quoteShell(exePath) &
            " s " & quoteShell(progfile)

  var output = ""
  var exitCode = -1
  try:
    let r = execCmdEx(cmd)
    output = r[0]
    exitCode = int(r[1])
  except:
    echo "Macro plugin: failed to invoke ", cmd
    return ""
  if exitCode != 0:
    echo "Error compiling macro plugin for '", cleanSymbolName(pool.syms[macroSym]), "':"
    echo output
    return ""

  result = exePath

proc runMacroPlugin*(nifcachePath: string; dest: var TokenBuf;
                     info: lineinfos.PackedLineInfo;
                     macroSym: SymId; args: TokenBuf): bool =
  let exePath = getMacroPluginPath(nifcachePath, macroSym)
  if not fileExists(exePath):
    echo "Macro plugin not found: ", exePath
    return false

  let inputPath = nifcachePath / "macro_in_" & $macroSym.int & ".nif"
  let outputPath = nifcachePath / "macro_out_" & $macroSym.int & ".nif"
  try:
    writeFile(inputPath, toString(args))
  except:
    echo "Macro plugin: failed to write ", inputPath
    return false

  let cmd = quoteShell(exePath) & " " & quoteShell(inputPath) & " " & quoteShell(outputPath)
  var output = ""
  var exitCode = -1
  try:
    let r = execCmdEx(cmd)
    output = r[0]
    exitCode = int(r[1])
  except:
    echo "Macro plugin: failed to invoke ", cmd
    return false
  if exitCode != 0:
    echo "Macro plugin execution failed:"
    echo output
    return false

  var s = nifstreams.open(outputPath)
  try:
    parse s, dest, lineinfos.NoLineInfo
  finally:
    close s
  return true
