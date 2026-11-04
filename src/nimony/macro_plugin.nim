#       Nimony Compiler
# (c) Copyright 2024-2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Macro plugin support: build the plugin module as NIF directly, hand it to
## `nimony s` (which picks up `.p.nif` input — same machinery `executeExpr`
## uses for CTFE), and exec the resulting binary at every call site.

import std/[syncio, os, osproc, tables, hashes, assertions, strutils]
import std/[dirs, paths]
include ".." / lib / compat2

import ".." / lib / [nifpools, bitabs, nifindexes, symparser]
import ".." / lib / nifreader
from ".." / lib / nifcoreparse import parse
import ".." / models / [tags]
import nimony_model, decls, programs
from semos import shellArgs, macroHostDir, macroHostStamp, pluginDepFile

type
  MacroPlugin* = object
    exePath*: string

func hash(s: SymId): Hash {.borrow.}

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
  copyInto dest, n:
    if n.hasMore and n.isTagLit and n.stmtKind == ResultS:
      # Skip the leading result declaration.
      skip n
    while n.hasMore:
      dest.takeTree n

proc rewriteSymsToIdentsImpl(newBuf: var TokenBuf; n: var Cursor) =
  ## Rewrites the single tree/token at `n` into `newBuf` and advances past it.
  if not n.hasMore: return
  case n.kind
  of Symbol, SymbolDef:
    newBuf.addIdent(pool.symNameId(n.symId), n.info)
    inc n
  of TagLit:
    let ek = n.exprKind
    var firstChild = n
    inc firstChild
    if (ek == OchoiceX or ek == CchoiceX) and firstChild.isSymbol:
      # unwrap the choice to a single ident:
      n.into:
        newBuf.addIdent(pool.symNameId(n.symId), n.info)
        while n.hasMore: skip n
    else:
      newBuf.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          rewriteSymsToIdentsImpl(newBuf, n)
        newBuf.addParRi(n.endInfo)
  else:
    newBuf.takeTree n

proc rewriteSymsToIdents(buf: var TokenBuf) =
  ## Convert every Symbol / SymbolDef in `buf` to an Ident bearing the symbol's
  ## base name, and unwrap `(ochoice …)` / `(cchoice …)` nodes the same way.
  ## After this pass, the buffer reads like nifler output — sem will resolve
  ## every name against the plugin module's own scope.
  var newBuf = createTokenBuf(buf.len)
  var n = beginRead(buf)
  rewriteSymsToIdentsImpl(newBuf, n)
  buf = ensureMove newBuf

# ----------------------------------------------------------------------------
# Plugin module generator
# ----------------------------------------------------------------------------

proc emitImportStdMacros(dest: var TokenBuf; info: NifLineInfo) =
  ## Emit `(import (infix / std (bracket syncio macros)))` — the same shape
  ## nifler emits for `import std/[syncio, macros]`.
  dest.copyInto(globalTags.registerTag("import"), info):
    dest.copyInto(globalTags.registerTag("infix"), info):
      dest.addIdent "/", info
      dest.addIdent "std", info
      dest.copyInto(globalTags.registerTag("bracket"), info):
        dest.addIdent "syncio", info
        dest.addIdent "macros", info

proc emitNimNodeRetType(dest: var TokenBuf; info: NifLineInfo) =
  ## Pre-sem the return type is just the ident `NimNode`. Sem resolves it
  ## against `lib/std/macros.nim`'s `NimNode*` type.
  dest.addIdent "NimNode", info

proc copyParamsRewritingMetatypes(dest: var TokenBuf; params: Cursor;
                                  info: NifLineInfo) =
  ## Copy a `(params (param ...)*)` subtree but rewrite each param's TYPE slot
  ## from `(untyped)` / `(typed)` to the ident `NimNode`. The user's macro
  ## signature uses `untyped` (for "any AST, don't sem-check") which is
  ## meaningful at the macro CALL site, but inside the macro body we want
  ## the parameter to be usable as a `NimNode` (so methods like `len`,
  ## `[i]`, `kind`, `add`, etc. resolve). This rewrite gives us both.
  ##
  ## Param shape per `(param :name pragmas TYPE default)`. We pass through
  ## name/pragmas/default verbatim and only swap the TYPE slot.
  assert params.substructureKind == ParamsU
  var n = params
  copyInto dest, n:
    while n.hasMore:
      if isParamLike(n.substructureKind):
        copyInto dest, n:
          # Slot 0: name (SymbolDef or Ident)
          dest.takeTree n
          # Slot 1: exported marker (DotToken)
          dest.takeTree n
          # Slot 2: pragmas
          dest.takeTree n
          # Slot 3: type — rewrite (untyped) / (typed) → NimNode
          let isMetatype = n.isTagLit and
            (n.typeKind == UntypedT or n.typeKind == TypedT)
          if isMetatype:
            dest.addIdent "NimNode", info
            skip n
          else:
            dest.takeTree n
          # Slot 4: default value
          dest.takeTree n
      else:
        # Non-param entry (e.g. return-type-of-routine slot at end). Copy verbatim.
        dest.takeTree n

proc emitImplProc(dest: var TokenBuf; implName: string; macroDecl: Cursor;
                  info: NifLineInfo) =
  ## Emit `(proc <implName> . . . (params <copied-and-rewritten>) NimNode . . <body>)`.
  let r = asRoutine(macroDecl, SkipInclBody)
  dest.copyInto(globalTags.registerTag("proc"), info):
    dest.addIdent implName, info
    dest.addEmpty3 info                       # exported, pattern, typevars
    # params: copy verbatim, but swap (untyped)/(typed) types for NimNode so
    # the body can call NimNode methods on them without losing the call-site
    # "don't sem-check the arg" semantics (which the user's macro signature
    # already provides via the untyped/typed metatype).
    if r.params.isDotToken:
      dest.copyInto(globalTags.registerTag("params"), info):
        discard
    else:
      copyParamsRewritingMetatypes(dest, r.params, info)
    emitNimNodeRetType(dest, info)
    dest.addEmpty2 info                       # pragmas, effects
    spliceBodyWithoutResult(dest, r.body)

proc emitMainProc(dest: var TokenBuf; implName: string; paramCount: int;
                  info: NifLineInfo) =
  ## Emit:
  ##   proc main =
  ##     let input = loadInput()
  ##     let arg0 = input[0]; let arg1 = input[1]; ...
  ##     let output = <implName>(arg0, arg1, ...)
  ##     saveOutput(output)
  let procTag = globalTags.registerTag("proc")
  let letTag = globalTags.registerTag("let")
  let callTag = globalTags.registerTag("call")
  let stmtsTag = globalTags.registerTag("stmts")
  let paramsTag = globalTags.registerTag("params")
  let bracketExprTag = globalTags.registerTag("at")

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
  if r.params.isDotToken or r.params.substructureKind != ParamsU:
    return 0
  var p = r.params
  p.loopInto:
    if isParamLike(p.substructureKind):
      inc result
    skip p

proc buildPluginNif*(macroDecl: Cursor; macroSym: SymId;
                     info: NifLineInfo): TokenBuf =
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
  let macroName = cleanSymbolName(pool.symString(macroSym))
  let implName = macroName & "Impl"
  let paramCount = countParams(macroDecl)

  result.copyInto(globalTags.registerTag("stmts"), info):
    emitImportStdMacros(result, info)
    emitImplProc(result, implName, macroDecl, info)
    emitMainProc(result, implName, paramCount, info)
    # call main()
    result.copyInto(globalTags.registerTag("call"), info):
      result.addIdent "main", info

  rewriteSymsToIdents(result)

# ----------------------------------------------------------------------------
# Driver: write the NIF, build with Nimony, exec at call sites
# ----------------------------------------------------------------------------

proc macroFileStem(macroSym: SymId): string =
  ## `macro_<sym>` with the symbol's full NIF name (module suffix included),
  ## made file-name safe. The name, not the `SymId` number: that is an index
  ## into one process's pool, so two nimsem processes of the same build (one
  ## per module, run side by side by `nifmake -j`) hand out the same numbers,
  ## and every file named after one was shared between unrelated macros —
  ## their plugin builds clobbered each other's host caches.
  let symName = pool.symString(macroSym)
  result = "macro_"
  for ch in symName:
    if ch in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
      result.add ch
    else:
      result.add '_'

proc getMacroPluginPath*(nifcachePath: string; macroSym: SymId): string =
  result = nifcachePath / macroFileStem(macroSym)
  when defined(windows):
    result.add ".exe"

proc macroPluginExists*(nifcachePath: string; macroSym: SymId): bool =
  ## True when a compiled plugin binary for `macroSym` already sits in the
  ## shared nifcache. Used to recognise a macro IMPORTED from another module:
  ## its declaration is never re-semchecked in the importer (so it is absent
  ## from `SemContext.compiledMacros`), but the dependency build compiled its
  ## plugin into the same nifcache we read here.
  fileExists(getMacroPluginPath(nifcachePath, macroSym))

proc hostToolArgs(hostCommandLineArgs, cacheDir: string): string =
  ## The options every host-cache sub-compile shares: the outer command line
  ## minus its target triple, plus `-d:nimonyMacroHost`. The define has to be
  ## on BOTH the setup and the per-macro `nimony s`, so that the two compute
  ## the same frontend key and the second finds the first's work current. It
  ## also keeps the setup, which imports `std/macros` itself, from scheduling
  ## a `macrohost` node of its own (`deps.macrosNode`).
  ##
  ## The outer compile's args (notably `--cc`) are forwarded so the nested
  ## build's nifmake-cmd signatures match the outer's. Otherwise nifmake's
  ## per-cmd staleness check sees a different argv for `nimsem ... m
  ## sysvq0asl.p.nif`, decides the existing `sysvq0asl.s.nif` is stale, and
  ## tries to overwrite it — which on Windows fails because the outer nimsem
  ## (currently paused waiting on this exec) still has it mmap'd. Same
  ## rationale as `semos.runProgram` / `semos.prepareEval`.
  let srcLibPath = getAppDir().parentDir() / "src" / "lib"
  result = quoteShell(getAppDir() / "nimony") & shellArgs(hostCommandLineArgs) &
           " -d:nimonyMacroHost" &
           " --path:" & quoteShell(srcLibPath) &
           " --nimcache:" & quoteShell(cacheDir)

proc setupHostCache(cacheDir, hostCommandLineArgs: string; depFile = ""): bool =
  ## Pre-populate the host cache `cacheDir` with a HOST-bits stdlib. `nimony s`
  ## only READS its imports' `.s.nif` — it does not build them — so the stdlib
  ## the plugin imports must be materialised first. The plugin scaffold
  ## imports exactly `std/[syncio, macros]` (see `emitImportStdMacros`);
  ## compiling those pulls in the whole host-bits stdlib closure the plugin
  ## needs (incl. the NimNode/NIF-reader machinery and the parser). The native
  ## link of this setup may fail (harmless — we only need the `.s.nif`/
  ## `.c.nif`), and `nimony c` is incremental so repeat calls are cheap.
  let setupFile = cacheDir / "macro_setup.nim"
  try:
    createDir path(cacheDir)
    writeFile(setupFile, "import std/[syncio, macros]\n")
  except:
    echo "Macro plugin: failed to write ", setupFile
    return false
  var setupCmd = hostToolArgs(hostCommandLineArgs, cacheDir)
  if depFile.len > 0:
    setupCmd.add " --depfile:"
    setupCmd.add quoteShell(depFile)
  setupCmd.add " c "
  setupCmd.add quoteShell(setupFile)
  try:
    discard execCmdEx(setupCmd)   # ignore exit: a failed native link still leaves the .s.nif/.c.nif
  except:
    echo "Macro plugin: failed to setup host stdlib: ", setupCmd
    return false
  result = true

proc buildMacroHost*(nifcachePath, hostCommandLineArgs: string) =
  ## `nimsem macrohost`: the build graph's node for `semos.macroHostDir`. It
  ## runs once, before every sem node that may compile a macro, so the setup
  ## never races a sibling (two of them building the parser's plugins in one
  ## cache would corrupt each other's incremental caches). The per-macro
  ## sub-compiles that follow only read it: their writes are per-macro files
  ## and the shared `.x.nif`, which `shoggoth` installs by rename.
  let stamp = macroHostStamp(nifcachePath)
  if not setupHostCache(macroHostDir(nifcachePath), hostCommandLineArgs,
                        pluginDepFile(stamp)):
    quit 1
  try:
    writeFile(stamp, "")
  except:
    quit "Macro plugin: failed to write " & stamp

proc compileMacroPlugin*(nifcachePath: string; macroDecl: Cursor; macroSym: SymId;
                         info: NifLineInfo;
                         hostCommandLineArgs: string; sharedHost: bool): string =
  ## Build the plugin module straight from NIF (no Nim text round-trip), write
  ## it as a `.p.nif`, and have Nimony compile it through `s` (the NIF-input
  ## entry point — same one CTFE uses in `semos.runEval`).
  ##
  ## `sharedHost` says the build graph already built `semos.macroHostDir`
  ## (`SharedMacroHost`). Without it — a module the graph did not see import
  ## `std/macros` — the plugin gets a host cache of its own and builds the
  ## stdlib there first, as every macro used to.
  let exePath = getMacroPluginPath(nifcachePath, macroSym)
  let pluginBaseName = macroFileStem(macroSym)

  # A macro plugin is a HOST-native executable, so it must be built with a
  # host-consistent toolchain config (host word size, host stdlib layouts). The
  # outer compile's nifcache may target a DIFFERENT word size — the JS backend
  # compiles at `--bits:32` so its Leng IR matches the JS runtime — and the
  # plugin's `nimony s` sub-compile reuses whatever stdlib artifacts already sit
  # in the nifcache it is pointed at. Sharing the outer nifcache would hand the
  # 64-bit plugin a 32-bit stdlib (mismatched type sizes) → a plugin that builds
  # but SEGFAULTS at run. So the plugin builds in a nimcache subdir of its own
  # at host bits: `hostCommandLineArgs` is the outer command line minus its
  # target triple (`parseCommonOption` decides what a compile-time-eval
  # process may see).
  let pluginCache = if sharedHost: macroHostDir(nifcachePath)
                    else: nifcachePath / pluginBaseName & ".host"
  try:
    createDir path(pluginCache)
  except:
    echo "Macro plugin: failed to create ", pluginCache
    return ""
  let progfile = pluginCache / pluginBaseName.addFileExt(".p.nif")

  var buf = buildPluginNif(macroDecl, macroSym, info)
  try:
    writeFileAndIndex(progfile, buf)
  except:
    echo "Macro plugin: failed to write ", progfile
    return ""

  # `nimony s` opens `<progfile>.p.deps.nif` unconditionally — write an empty
  # `(stmts)` (the plugin module has no external NIF dependencies of its own,
  # only stdlib imports which Nimony discovers via its normal search path).
  let depsFile = pluginCache / pluginBaseName & ".p.deps.nif"
  var deps = createTokenBuf(4)
  deps.addParLe StmtsS, info
  deps.addParRi()
  try:
    writeFile(depsFile, toString(deps, true))
  except:
    echo "Macro plugin: failed to write ", depsFile
    return ""

  if not sharedHost and not setupHostCache(pluginCache, hostCommandLineArgs):
    return ""

  # `--nimcache:` (in `hostToolArgs`) so the sub-compile reads the
  # `.p.deps.nif` from the directory we wrote it to. Without it, `nimony s`
  # falls back to its default `nimcache/` and can't find the deps file under
  # parallel test execution (CI uses `nimcache/.par/<n>/` per worker).
  let cmd = hostToolArgs(hostCommandLineArgs, pluginCache) &
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
    echo "Error compiling macro plugin for '", cleanSymbolName(pool.symString(macroSym)), "':"
    echo output
    return ""

  result = exePath

proc runMacroPlugin*(nifcachePath: string; dest: var TokenBuf;
                     info: NifLineInfo;
                     macroSym: SymId; args: TokenBuf): bool =
  let exePath = getMacroPluginPath(nifcachePath, macroSym)
  if not fileExists(exePath):
    echo "Macro plugin not found: ", exePath
    return false

  let stem = macroFileStem(macroSym)
  let inputPath = nifcachePath / stem & ".in.nif"
  let outputPath = nifcachePath / stem & ".out.nif"
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

  var r = nifreader.open(outputPath)
  # A `NimNode` has no position, so the output has none of its own: every
  # node is the macro call's, which is where Nim reports an error in it too.
  parse(r, dest, parentSeed = info, denseLineInfo = true)
  r.close()
  return true
