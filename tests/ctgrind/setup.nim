## Custom runner for the constant-time evidence of RSA signing (ctgrind):
## valgrind memcheck with the secrets marked undefined reports every branch
## and every memory address computed from one. Two drivers in `deps/`:
##
## - `ctbignum_ctgrind`: `std/crypto/private/ctbignum`'s key setup, CRT
##   private operation and fault check, from p, q, dP, dQ, qInv and the
##   blinding inputs. Memcheck must report NOTHING.
## - `rsasign_ctgrind`: the public API from the PEM text to the signature
##   (`ctBase64Decode` and the scans of `parseRsaPrivateKeyPem`,
##   `parseRsaPrivateKeyDer`, `rsaSignPkcs1v15`). A
##   computation on secrets ends in a few public facts, and memcheck reports
##   where they are branched on; every report must be one of the intended
##   declassification points of `allowed` below, in the driver stage it
##   belongs to.
##
## Each is built for the C backend (`nimony c -d:release`, the portable
## twins at -O3, client requests from `<valgrind/memcheck.h>`) and, on
## Linux x86-64 with arkham present, the native backend (`nimony n
## -d:release`, the `{.assembler.}` primitives; secrets tainted, see the
## drivers). Each is also built with `-d:ctgrindSelfTest`, which adds a
## deliberate leak: that build must FAIL the same check, which shows the
## marking reaches the computation and the check is not vacuous.
##
## A third program, `ctcount`, signs with two keys and two messages under
## callgrind, counting only inside `rsaCrtRaw` (the CRT private operation)
## and `rsaCrtCheckMask` (the fault check), whose instruction, data-access
## and branch counts must be the same for every key, message and (random)
## blinding factor.
##
## Skipped, successfully, when valgrind or its headers are not installed.
import std / [os, osproc, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let dir = if arg("dir").len > 0: arg("dir") else: "tests/ctgrind"
let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
let cachedir = if arg("cachedir").len > 0: arg("cachedir") else: "nimcache"
let forward = arg("forward")
var cc = "gcc"
for f in forward.splitWhitespace:
  if f.startsWith("--cc:"): cc = f.substr(5)

# The one declassification of the C builds (`-d:ctgrind`, see `CFlags`)
# must stay narrow: memcheck is told the bit length of a secret is public
# only inside `publicBitLen`, which only `decodeModulus` calls, which is
# called on the moduli it is documented for (a `CtMont` modulus, and p and q
# in `initRsaCrt` and `rsaCrtRaw`): five sites. A new call site makes this
# check fail on purpose, so that it gets reviewed before it can hide a leak.
block declassification:
  let port = dir / ".." / ".." / "lib" / "std" / "crypto" / "private" / "ctbignumport.nim"
  var defined = 0
  var problems: seq[string] = @[]
  for f in walkDirRec(dir / ".." / ".." / "lib"):
    if f.endsWith(".nim"):
      let n = readFile(f).count("VALGRIND_MAKE_MEM_DEFINED")
      if n > 0 and not f.sameFile(port):
        problems.add f & " declares memory defined"
      defined += n
  let src = readFile(port)
  if defined != 1: problems.add "VALGRIND_MAKE_MEM_DEFINED: " & $defined & " uses, expected 1"
  if src.count("makeMemDefined(") != 2:
    problems.add "makeMemDefined: expected only its declaration and the call in publicBitLen"
  if src.count("publicBitLen(") != 2:
    problems.add "publicBitLen: expected only its definition and the call in decodeModulus"
  if src.count("decodeModulus(") != 6:
    problems.add "decodeModulus: " & $(src.count("decodeModulus(") - 1) &
                 " call sites, expected 5 (review any new one)"
  if problems.len > 0:
    echo "FAILURE tests/ctgrind: declassification sites changed:\n  ", problems.join("\n  ")
    quit 1

if findExe("valgrind").len == 0:
  echo "SKIP tests/ctgrind: valgrind is not installed"
  quit 0
let (_, hdrCode) = execCmdEx("printf '#include <valgrind/memcheck.h>\\n' | " &
                             cc.quoteShell & " -E -x c - -o /dev/null")
if hdrCode != 0:
  echo "SKIP tests/ctgrind: <valgrind/memcheck.h> is not installed"
  quit 0

let work = absolutePath(cachedir / "ctgrind")
createDir work
let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
var failures = 0

type
  Report = object
    kind: string            # memcheck's first line: "Conditional jump ...", ...
    frames: seq[string]     # innermost first, Nimony's names without suffixes

proc plainName(frame: string): string =
  ## `ctBase64Decode_0_pem0dvagj` (C) or `ctBase64Decode.0.pem0dvagj`
  ## (native) to `ctBase64Decode`.
  var s = frame
  let c = s.find(": ")
  if c >= 0: s = s.substr(c + 2)
  let paren = s.find(" (")
  if paren >= 0: s.setLen paren
  let dot = s.find('.', 1)
  if dot > 0: s.setLen dot
  var i = 0
  while i + 2 < s.len:
    if s[i] == '_' and s[i+1] in Digits:
      var j = i + 1
      while j < s.len and s[j] in Digits: inc j
      if j < s.len and s[j] == '_':
        s.setLen i
        break
    inc i
  result = s

proc parseLog(log: string): seq[Report] =
  ## The error blocks of a `valgrind -q --log-file` log.
  result = @[]
  var cur = Report(kind: "", frames: @[])
  for raw in log.splitLines:
    var line = raw
    if line.startsWith("=="):
      let e = line.find("== ", 2)
      line = if e >= 0: line.substr(e + 3) else: ""
    if line.strip.len == 0:
      if cur.kind.len > 0: result.add cur
      cur = Report(kind: "", frames: @[])
    elif line.startsWith("   at ") or line.startsWith("   by "):
      cur.frames.add plainName(line)
    elif cur.kind.len == 0:
      cur.kind = line.strip
  if cur.kind.len > 0: result.add cur

proc allowed(r: Report): bool =
  ## The intended declassification points of `rsasign_ctgrind`.
  if r.frames.len == 0: return false
  let top = r.frames[0]
  let branch = r.kind.startsWith("Conditional jump or move depends on uninitialised")
  let address = r.kind.startsWith("Use of uninitialised value")
  if "stageArmour" in r.frames:
    # A: the body's LAYOUT (how many letters, where the `=` and the line
    # breaks are) is public, but memcheck cannot tell a character's class
    # from its value. It surfaces where the decoder counts letters and
    # padding, moves its output position, sizes and clears the result, and
    # gives its verdict.
    result = (branch or address) and
             (top in ["ctBase64Decode", "setLen", "=destroy", "eqdestroy"] or
              (top == "stageArmour" and branch))
  elif "stageScan" in r.frames:
    # A': the END-line and header scans over the body. `ctFindByte` must be
    # silent; `ctContinuesWith` gives a verdict its caller branches on, and
    # arkham lowers the final `!= 0` to a `bool` with a jump, not `setcc`.
    result = branch and top == "ctContinuesWith"
  elif "stageKey" in r.frames:
    # B: whether the key is valid (its INTEGERs minimal, p and q odd and of
    # the right width, the CRT values in range, p * q == n).
    result = branch and top in ["parsePkcs1", "parseDer"]
  elif "stageSign" in r.frames:
    # C: the fault check's verdict. `s^e mod n` is constant time
    # (`rsaCrtCheckMask`); only the final all-ones test is branched on.
    result = branch and top in ["rsaCrtCheck", "rsaCrtPrivate"]
  else:
    result = false

proc build(backend, driver, tag, flags: string): string =
  ## The executable, or "" when the build failed (reported).
  let cache = work / tag
  let cmd = nimony & " " & backend & " --silentMake --nimcache:" & quoteShell(cache) &
            " " & flags & " " & forward & " " & quoteShell(dir / "deps" / driver & ".nim")
  let (output, code) = execCmdEx(cmd)
  result = ""
  if code != 0:
    echo "FAILURE: ", cmd, "\n", output
    inc failures
    return
  for f in walkDirRec(cache / backend):
    if f.extractFilename == driver: result = f
  if result.len == 0:
    echo "FAILURE: no executable for ", cmd
    inc failures

proc ccFlag(compiler: string): string =
  if compiler.len > 0: " --cc:" & compiler else: ""

proc check(backend, compiler, driver, flags: string; selfTest: bool) =
  let tag = backend & compiler & "_" & driver & (if selfTest: "_self" else: "")
  let exe = build(backend, driver, tag,
                  flags & ccFlag(compiler) & (if selfTest: " -d:ctgrindSelfTest" else: ""))
  if exe.len == 0: return
  let log = work / tag & ".vg.log"
  let (output, _) = execCmdEx("valgrind -q --num-callers=24 --log-file=" &
                              quoteShell(log) & " " & quoteShell(exe))
  let reports = parseLog(readFile(log))
  var bad: seq[Report] = @[]
  for r in reports:
    if driver == "ctbignum_ctgrind" or not allowed(r): bad.add r
  let what = "`nimony " & backend & ccFlag(compiler) & "` " & driver &
             (if selfTest: " (self-test)" else: "")
  if "ctgrind ok" notin output:
    echo "FAILURE tests/ctgrind: ", what, " computed wrong results:\n", output
    inc failures
  elif selfTest and bad.len == 0:
    echo "FAILURE tests/ctgrind: ", what, ": the deliberate leak was not reported"
    inc failures
  elif not selfTest and bad.len > 0:
    echo "FAILURE tests/ctgrind: ", what, ": ", bad.len,
         " report(s) outside the declassification points, see ", log
    for r in bad:
      echo "  ", r.kind, "\n    ", r.frames.join(" < ")
    inc failures
  else:
    echo "SUCCESS tests/ctgrind: ", what, ": ", reports.len, " report(s), ",
         bad.len, (if selfTest: " leak(s) caught" else: " outside the declassification points")

proc counts(backend, compiler, flags: string) =
  ## The callgrind totals of `rsaCrtRaw` and `rsaCrtCheckMask` must not
  ## depend on the key, the message or the blinding.
  let exe = build(backend, "ctcount", backend & compiler & "_ctcount", flags & ccFlag(compiler))
  if exe.len == 0: return
  const
    Events = ["Ir", "Dr", "Dw", "Bc", "Bi"]
    Messages = ["hello nimony", "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiIxIn0"]
  var first = ""
  var runs = 0
  for key in ["test2048.pkcs8.pem", "test2048b.pkcs8.pem"]:
    for msg in Messages:
      let outFile = work / backend & compiler & "_ctcount.callgrind"
      # the inputs on stdin, so that every run starts from the same stack
      # (see `deps/ctcount.nim`)
      let cmd = "printf '%s\\n%s\\n' " &
                quoteShell(dir / ".." / "nimony" / "crypto" / "rsakeys" / key) & " " &
                quoteShell(msg) & " | " &
                "valgrind -q --tool=callgrind --toggle-collect=" & quoteShell("rsaCrtRaw*") &
                " --toggle-collect=" & quoteShell("rsaCrtCheckMask*") &
                " --cache-sim=yes --branch-sim=yes --callgrind-out-file=" &
                quoteShell(outFile) & " " & quoteShell(exe)
      let (output, code) = execCmdEx(cmd)
      if code != 0 or "ok" notin output:
        echo "FAILURE tests/ctgrind: ", cmd, "\n", output
        inc failures
        return
      var names, totals: seq[string] = @[]
      for line in lines(outFile):
        if line.startsWith("events: "): names = line.substr(8).splitWhitespace
        elif line.startsWith("totals: "): totals = line.substr(8).splitWhitespace
      var got = ""
      for e in Events:
        let i = names.find(e)
        got.add " " & e & "=" & (if i >= 0 and i < totals.len: totals[i] else: "0")
      if runs == 0: first = got
      elif got != first:
        echo "FAILURE tests/ctgrind: `nimony ", backend, ccFlag(compiler), "` rsaCrtRaw+rsaCrtCheckMask counts differ:\n  ",
             first, "\n  ", got, " (", key, ", ", msg, ")"
        inc failures
        return
      inc runs
  if not first.contains("Ir=") or first.contains("Ir=0 "):
    echo "FAILURE tests/ctgrind: `nimony ", backend, "` rsaCrtRaw+rsaCrtCheckMask was not counted: ", first
    inc failures
  else:
    echo "SUCCESS tests/ctgrind: `nimony ", backend, ccFlag(compiler), "` rsaCrtRaw+rsaCrtCheckMask counts equal over ",
         runs, " runs:", first

# The C compiler decides whether a mask stays a mask, so the C builds run
# with the configured compiler and, when it is installed and not already
# that, with clang too: clang -O3 once turned ctbignum's masks into branches
# that gcc -O3 kept.
var compilers = @[""]
if findExe("clang").len > 0 and cc != "clang": compilers.add "clang"

# `-d:ctgrind`: ctbignum's portable path (BearSSL's i31 code) declares the
# bit lengths of p and q public, as BearSSL does, and tells memcheck so at
# that one point (`publicBitLen`, through `decodeModulus`, in
# lib/std/crypto/private/ctbignumport.nim; see the check at the top).
const CFlags = "-d:release -d:useLibc -d:ctgrind --passC:-DMI_TRACK_VALGRIND=1"
for driver in ["ctbignum_ctgrind", "rsasign_ctgrind"]:
  for selfTest in [false, true]:
    for compiler in compilers:
      check "c", compiler, driver, CFlags, selfTest
    when defined(linux) and defined(amd64):
      if fileExists(bindir / "arkham".addFileExt(ExeExt)):
        check "n", "", driver, "-d:release", selfTest
for compiler in compilers:
  counts "c", compiler, "-d:release -d:useLibc"
when defined(linux) and defined(amd64):
  if fileExists(bindir / "arkham".addFileExt(ExeExt)):
    counts "n", "", "-d:release"
if failures > 0: quit 1
