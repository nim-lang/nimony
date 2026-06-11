
import std / [strutils, sequtils, tables, os, osproc]

# --- Windows symbol resolution --------------------------------------------
# The POSIX column lists symbolic `E*` names that are resolved against
# `<errno.h>` via `importc` when `errorcodes_posix.nim` is compiled. The
# Windows column instead carries symbolic `ERROR_*` names (or, for the socket
# `WSA*` numbers that have no portable spelling, plain integer literals). We
# can't leave `ERROR_*` as `importc` vars in the output: the reverse
# `errorCodeToWindows` map would still work, but the values would then depend on
# `<windows.h>` at every consumer (including the header-free nimony build). So
# instead we resolve the symbols to their real integers *here*, at generation
# time, by emitting a tiny Nim program that includes `<windows.h>`, prints each
# symbol's value, compiling and running it, and baking the results back as
# literal `int32`s. Run this generator on Windows whenever the Windows column
# gains a new symbolic name.

proc isNumericToken(s: string): bool =
  ## True for `534`, `534'i32`, ... (a value that needs no resolution).
  var t = s.strip
  if t.endsWith("'i32"): t = t[0 ..< t.len - 4]
  if t.len == 0: return false
  for c in t:
    if c notin {'0'..'9'}: return false
  result = true

proc resolveWindowsSymbols(symbols: seq[string]): Table[string, string] =
  ## Maps each `ERROR_*` symbol to its `<value>'i32` literal by compiling and
  ## running a generated probe. Quits if a symbol cannot be resolved.
  result = initTable[string, string]()
  if symbols.len == 0: return
  var prog = ""
  for s in symbols:
    prog.add "var " & s & " {.importc: \"" & s & "\", header: \"<windows.h>\".}: int32\n"
  for s in symbols:
    prog.add "echo \"" & s & " \", " & s & "\n"
  let tmpNim = getTempDir() / "ec_winresolve.nim"
  let exe = getTempDir() / "ec_winresolve".addFileExt(ExeExt)
  writeFile(tmpNim, prog)
  let (compileOut, code) = execCmdEx("nim c --hints:off -o:" & quoteShell(exe) & " " & quoteShell(tmpNim))
  if code != 0:
    quit "could not compile Windows symbol resolver:\n" & compileOut
  let (runOut, runCode) = execCmdEx(quoteShell(exe))
  if runCode != 0:
    quit "could not run Windows symbol resolver:\n" & runOut
  for line in runOut.splitLines:
    let parts = line.strip.split(' ')
    if parts.len == 2:
      result[parts[0]] = parts[1].strip & "'i32"
  for s in symbols:
    if not result.hasKey(s):
      quit "Windows symbol not resolved (run the generator on Windows): " & s

proc windowsColumnSymbols(values: string): seq[string] =
  ## The non-numeric tokens of a Windows column cell (the ones to resolve).
  result = @[]
  for v in values.strip.split(", ").filterIt(it.strip.len > 0):
    let t = v.strip
    if not isNumericToken(t): result.add t

proc resolveWindowsColumn(values: string; table: Table[string, string]): string =
  ## Rewrites a Windows column cell, replacing symbolic names with their
  ## resolved literals and passing numeric literals through unchanged.
  var outVals: seq[string] = @[]
  for v in values.strip.split(", ").filterIt(it.strip.len > 0):
    let t = v.strip
    if isNumericToken(t): outVals.add t
    else: outVals.add table[t]
  result = outVals.join(", ")

# --- POSIX errno resolution (per-OS, cached) -------------------------------
# Unlike Windows, errno numbers differ across POSIX OSes (Linux/macOS/BSD), so
# they cannot be baked as one literal set. Each value is captured *per OS* into
# `tools/errno_cache.txt` by running this generator on that OS; the host's slice
# is refreshed on every run. `errorcodes_posix.nim` then emits a
# `when defined(<os>): <literals>` block per cached OS plus an `else:` branch
# that keeps the original behaviour — resolving against `<errno.h>` via `importc`
# at the consumer's compile time — for any not-yet-baked OS. With an empty cache
# the output is byte-for-byte the importc-only version, so baking an OS is purely
# additive.

# Paths are relative to the repo root (run as `nim c -r tools/gen_errorcodes.nim`,
# matching tools/gen_tags.nim). The errorcodes sources live in-tree under
# `lib/std/errorcodes` (no longer a vendored submodule).
const ErrorcodesDir = "lib/std/errorcodes"
const ErrnoCachePath = ErrorcodesDir / "errno_cache.txt"

proc detectCC(): string =
  for c in ["cc", "gcc", "clang"]:
    if findExe(c).len > 0: return c
  quit "no C compiler (cc/gcc/clang) on PATH for errno resolution"

proc resolveErrno(symbols: seq[string]): OrderedTable[string, string] =
  ## Compiles and runs an `<errno.h>` probe. Each symbol is `#ifdef`-guarded so
  ## names absent on this OS are simply skipped rather than breaking the build.
  result = initOrderedTable[string, string]()
  if symbols.len == 0: return
  var c = "#include <stdio.h>\n#include <errno.h>\nint main(void){\n"
  for s in symbols:
    c.add "#ifdef " & s & "\n  printf(\"" & s & " %d\\n\", (int)" & s & ");\n#endif\n"
  c.add "  return 0;\n}\n"
  let tmpC = getTempDir() / "ec_errno.c"
  let exe = getTempDir() / "ec_errno".addFileExt(ExeExt)
  writeFile(tmpC, c)
  let cc = detectCC()
  let (cout, code) = execCmdEx(cc & " -o " & quoteShell(exe) & " " & quoteShell(tmpC))
  if code != 0: quit "errno probe failed to compile:\n" & cout
  let (rout, rcode) = execCmdEx(quoteShell(exe))
  if rcode != 0: quit "errno probe failed to run:\n" & rout
  for line in rout.splitLines:
    let p = line.strip.split(' ')
    if p.len == 2: result[p[0]] = p[1].strip

proc loadErrnoCache(): OrderedTable[string, OrderedTable[string, string]] =
  result = initOrderedTable[string, OrderedTable[string, string]]()
  if not fileExists(ErrnoCachePath): return
  var cur = ""
  for line in lines(ErrnoCachePath):
    let s = line.strip
    if s.len == 0 or s.startsWith("#"): continue
    if s.startsWith("os "):
      cur = s[3 .. ^1].strip
      result[cur] = initOrderedTable[string, string]()
    elif cur.len > 0:
      let p = s.split(' ')
      if p.len == 2: result[cur][p[0]] = p[1].strip

proc saveErrnoCache(cache: OrderedTable[string, OrderedTable[string, string]]) =
  var s = "# errno integer values captured per OS by running gen_errorcodes.nim\n" &
          "# on that OS. The <errno.h> constants are an OS/ABI detail, so they are\n" &
          "# baked per-OS here instead of pulled via <errno.h> at the consumer.\n" &
          "# Auto-generated; do not edit by hand.\n"
  for osName, syms in cache:
    s.add "os " & osName & "\n"
    for k, v in syms:
      s.add k & " " & v & "\n"
  writeFile(ErrnoCachePath, s)

proc posixSymbols(cell: string): seq[string] =
  ## The `E*` symbols of a POSIX column cell.
  result = @[]
  for v in cell.strip.split(", ").filterIt(it.strip.len > 0):
    let t = v.strip
    if t.len > 0 and t[0] == 'E': result.add t

proc mapPosixCell(cell: string; values: OrderedTable[string, string]): string =
  ## A POSIX cell rewritten to baked `<value>'i32` literals, dropping symbols
  ## this OS lacks and de-duplicating values that coincide (e.g. EAGAIN ==
  ## EWOULDBLOCK on Linux).
  var outv: seq[string] = @[]
  for v in cell.strip.split(", ").filterIt(it.strip.len > 0):
    let t = v.strip
    if values.hasKey(t):
      let lit = values[t] & "'i32"
      if lit notin outv: outv.add lit
  result = outv.join(", ")

proc elifSection(dest, destB: var string; values, enumVal: string; sections: var int) =
  let vals = values.strip.split(", ").filterIt(it.strip.len > 0)
  if vals.len > 0:
    inc sections
    let keyw = if sections == 1: "  if" else: "  elif"
    dest.add keyw & " err == "
    let firstVal = vals[0].strip
    dest.add firstVal
    for j in 1 ..< vals.len:
      dest.add " or err == "
      dest.add vals[j].strip
    dest.add ":\n"
    dest.add "    "
    dest.add enumVal
    dest.add "\n"

    destB.add "  of "
    destB.add enumVal
    destB.add ": "
    destB.add firstVal
    destB.add "\n"

proc addDecls(dest: var string; values: string) =
  let vals = values.strip.split(", ").filterIt(it.strip.len > 0)
  if vals.len > 0:
    for val in vals:
      let v = val.strip
      if v.len > 0 and v[0] == 'E':
        dest.add "var "
        dest.add v
        dest.add " {.importc: \""
        dest.add v
        dest.add "\", header: \"<errno.h>\".}: int32\n"

proc buildPosixProcs(rows: seq[seq[string]]; values: OrderedTable[string, string]): string =
  ## The two POSIX maps for a single baked OS, using its cached errno literals.
  var fwd = "proc posixToErrorCode*(err: int32): ErrorCode =\n"
  var rev = "proc errorCodeToPosix*(err: ErrorCode): int32 =\n  case err\n"
  var sections = 0
  for parts in rows:
    fwd.elifSection rev, mapPosixCell(parts[1], values), parts[0].strip, sections
  fwd.add "  else:\n    Failure\n"
  rev.add "  else: 1'i32\n"
  result = fwd & "\n" & rev

const
  Header = "# Generated by tools/gen_errorcodes.nim. DO NOT EDIT!\n"

proc importErrorCodes(): string =
  result = """
when not defined(nimony):
  import errorcodes
"""

proc main =
  var inp = open(ErrorcodesDir / "errorcodes.md", fmRead)
  var enumDecl = """
type
  ErrorCode* = enum ## All possible errors in one enum."""

  var errnoDecls = ""

  var fromPosix = """proc posixToErrorCode*(err: int32): ErrorCode =
"""

  var fromWindows = """proc windowsToErrorCode*(err: int32): ErrorCode =
"""

  var fromHttp = """proc httpToErrorCode*(err: int): ErrorCode =
"""

  var toPosix = """proc errorCodeToPosix*(err: ErrorCode): int32 =
  case err
"""

  var toHttp = """proc errorCodeToHttp*(err: ErrorCode): int =
  case err
"""

  var toWindows = """proc errorCodeToWindows*(err: ErrorCode): int32 =
  case err
"""

  # Read every data row up front so we can collect (and resolve) all symbolic
  # Windows names in a single probe before emitting any code.
  var rows: seq[seq[string]] = @[]
  var i = 0
  for line in lines(inp):
    inc i
    if i <= 2: continue # skip header
    var parts = line.split("|")
    if parts.len == 0: continue
    if parts.len != 7:
      quit "WRONG LINE: " & line
    parts = parts[1..^2] # ignore first and last entries which exist to due the surrounding |
    rows.add parts
  inp.close()

  # The Windows column carries symbolic `ERROR_*` names that can only be
  # resolved against `<windows.h>`, i.e. on a Windows host. Win32 error codes
  # are an ABI-stable constant set (identical on every Windows machine/SDK), so
  # the baked literals are host-invariant and `errorcodes_windows.nim` is only
  # ever imported under `when defined(windows)` anyway. On a non-Windows host we
  # therefore skip the probe and leave the committed Windows file untouched,
  # keeping the generator runnable everywhere (POSIX stays correct because it is
  # resolved at the *consumer's* compile time via `importc`, not baked here).
  const onWindowsHost = defined(windows)
  var winSymbols: seq[string] = @[]
  for parts in rows:
    for s in windowsColumnSymbols(parts[3]):
      if s notin winSymbols: winSymbols.add s
  var winValues = initTable[string, string]()
  when onWindowsHost:
    winValues = resolveWindowsSymbols(winSymbols)

  # POSIX errno is the mirror image of Windows: the values differ per OS, so
  # they're resolved at runtime against `<errno.h>` and cached per OS. On a
  # POSIX host, refresh this host's slice; on Windows (no POSIX consumer) leave
  # the cache as-is. `errorcodes_posix.nim` is emitted from whatever the cache
  # holds (empty cache => the original importc-only output).
  var errnoCache = loadErrnoCache()
  when hostOS != "windows":
    var posixSyms: seq[string] = @[]
    for parts in rows:
      for s in posixSymbols(parts[1]):
        if s notin posixSyms: posixSyms.add s
    let resolved = resolveErrno(posixSyms)
    if resolved.len > 0:
      errnoCache[hostOS] = resolved
      saveErrnoCache(errnoCache)
  var bakedOSes: seq[string] = @[]
  for osName, syms in errnoCache:
    if syms.len > 0: bakedOSes.add osName

  var posixSections = 0
  var httpSections = 0
  var windowsSections = 0
  for parts in rows:
    enumDecl.add "\n    "
    let enumVal = parts[0].strip
    enumDecl.add enumVal
    enumDecl.add "  ## "
    enumDecl.add parts[4].strip

    errnoDecls.addDecls parts[1]

    fromPosix.elifSection toPosix, parts[1], enumVal, posixSections
    fromHttp.elifSection toHttp, parts[2], enumVal, httpSections
    when onWindowsHost:
      fromWindows.elifSection toWindows, resolveWindowsColumn(parts[3], winValues), enumVal, windowsSections

  fromPosix.add """  else:
    Failure"""

  fromHttp.add """  else:
    Failure"""

  fromWindows.add """  else:
    Failure"""

  toPosix.add """  else: 1'i32"""

  toHttp.add """  else: 500"""

  toWindows.add """  else: 1124'i32"""

  var outf = open(ErrorcodesDir / "errorcodes.nim", fmWrite)
  outf.writeLine Header
  outf.writeLine enumDecl
  outf.close

  outf = open(ErrorcodesDir / "errorcodes_posix.nim", fmWrite)
  outf.writeLine Header
  outf.writeLine importErrorCodes()
  if bakedOSes.len == 0:
    # No OS baked yet: emit exactly the importc-only layout (header-compatible).
    outf.writeLine errnoDecls
    outf.writeLine ""
    outf.writeLine fromPosix
    outf.writeLine ""
    outf.writeLine toPosix
  else:
    # `when defined(<os>):` baked literals per cached OS, then an importc
    # fallback for everything else. The `importc` errno decls live only in the
    # fallback branch, so a baked OS never pulls `<errno.h>`.
    var body = ""
    for idx, osName in bakedOSes:
      let kw = if idx == 0: "when" else: "elif"
      body.add kw & " defined(" & osName & "):\n"
      body.add indent(buildPosixProcs(rows, errnoCache[osName]), 2)
      body.add "\n"
    body.add "else:\n"
    let importcVariant = errnoDecls & "\n" & fromPosix & "\n\n" & toPosix & "\n"
    body.add indent(importcVariant, 2)
    outf.writeLine body
  outf.close

  outf = open(ErrorcodesDir / "errorcodes_http.nim", fmWrite)
  outf.writeLine Header
  outf.writeLine importErrorCodes()
  outf.writeLine fromHttp
  outf.writeLine ""
  outf.writeLine toHttp
  outf.close

  when onWindowsHost:
    outf = open(ErrorcodesDir / "errorcodes_windows.nim", fmWrite)
    outf.writeLine Header
    outf.writeLine importErrorCodes()
    outf.writeLine fromWindows
    outf.writeLine ""
    outf.writeLine toWindows
    outf.close
  else:
    if winSymbols.len > 0:
      echo "note: not a Windows host — left errorcodes_windows.nim untouched ",
           "(rerun on Windows to refresh ", winSymbols.len, " symbolic value(s))"

main()
