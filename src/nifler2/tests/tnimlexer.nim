## Token-level tests for `nimlexer`.
##
## Every expectation in here was produced by `bin/refdump`, which runs Nim 2's
## own `compiler/lexer.nim` -- these are not guesses about what Nim does --
## except `caseSensitiveKeywords`, where Nimony parts ways with Nim. The
## sweep over whole files lives in `src/nifler2/tools/lexdiff.sh`; this file is
## the small, fast half that says *which* construct broke.

import std / [syncio, assertions]
import ".." / nimlexer

proc esc(s: string): string =
  result = ""
  var i = 0
  while i < s.len:
    let c = s[i]
    if c == '\n': result.add "\\n"
    elif c == '\r': result.add "\\r"
    elif c == '\t': result.add "\\t"
    elif c == '\\': result.add "\\\\"
    else: result.add c
    inc i

proc text(tok: Token): string =
  if tok.kind == tkSymbol: nimIdentNormalize(tok.s)
  elif tok.kind >= KeywordLow and tok.kind <= KeywordHigh: ""
  elif tok.kind in {tkOpr, tkColon, tkColonColon, tkEquals, tkDot, tkDotDot}:
    tok.s
  elif tok.kind >= tkIntLit and tok.kind <= tkCustomLit: tok.s
  elif tok.kind == tkComment: tok.s
  else: ""

proc render(src: string): string =
  result = ""
  for tok in tokens(src, "t.nim"):
    if tok.kind == tkEof: break
    if result.len > 0: result.add " "
    result.add $tok.kind
    let t = text(tok)
    if t.len > 0:
      result.add "("
      result.add esc(t)
      result.add ")"
    if tok.indent >= 0:
      result.add "@"
      result.add $tok.indent
    var sp = ""
    if tsLeading in tok.spacing: sp.add "L"
    if tsTrailing in tok.spacing: sp.add "T"
    if tsEof in tok.spacing: sp.add "E"
    if sp.len > 0:
      result.add "/"
      result.add sp

var failures = 0

proc check(name, src, expected: string) =
  let got = render(src)
  if got == expected:
    echo "ok ", name
  else:
    inc failures
    echo "FAIL ", name
    echo "  got:      ", got
    echo "  expected: ", expected

proc main =
  check "keywords", "if x: y\n",
    "if tkSymbol(x)/L :(:)/T tkSymbol(y)/L"
  # Nimony's keywords are case-sensitive; Nim would make the first two `proc`
  check "caseSensitiveKeywords", "p_roc pRoC Proc proc\n",
    "tkSymbol(proc) tkSymbol(proc)/L tkSymbol(Proc)/L proc/L"
  check "keywordBeforeUnicodeOpr", "and∙b\n",
    "and tkOpr(∙) tkSymbol(b)"
  check "suffixes", "1'I8 2'big 0x1f32 3'f32x 4U16 0xFF'f\n",
    "tkInt8Lit(1) tkCustomLit(2'big)/L tkIntLit(0x1f32)/L tkCustomLit(3'f32x)/L tkUInt16Lit(4)/L tkFloat32Lit(0xFF)/L"
  check "ints", "0 12 1_000 0xFF 0b1010 0o17\n",
    "tkIntLit(0) tkIntLit(12)/L tkIntLit(1_000)/L tkIntLit(0xFF)/L tkIntLit(0b1010)/L tkIntLit(0o17)/L"
  check "bigInt", "0xffffffff 2147483648 2147483647\n",
    "tkInt64Lit(0xffffffff) tkInt64Lit(2147483648)/L tkIntLit(2147483647)/L"
  check "floats", "1.5 1e5 2.5e-3 1'f32 3.0'f64\n",
    "tkFloatLit(1.5) tkFloatLit(1e5)/L tkFloatLit(2.5e-3)/L tkFloat32Lit(1)/L tkFloat64Lit(3.0)/L"
  check "intSuffix", "1'i8 12u8 5i64\n",
    "tkInt8Lit(1) tkUInt8Lit(12)/L tkInt64Lit(5)/L"
  check "basePrefixCase", "0X10 0B10 0O7 0C7\n",
    "tkIntLit(0x10) tkIntLit(0b10)/L tkIntLit(0)/L tkSymbol(O7) tkIntLit(0c7)/L"
  check "emptyBase", "0x 0b\n",
    "tkIntLit(0x) tkIntLit(0b)/L"
  check "customLit", "12'big\n",
    "tkCustomLit(12'big)"
  check "dotdot", "1..2\n",
    "tkIntLit(1) ..(..) tkIntLit(2)"
  check "digraphs", "{. .} [. .] (. .) [: ::\n",
    "{. .}/L [./L .]/L (./L .)/L [:/L ::(::)/LE"
  check "parDotVsDotDot", "(..2)\n",
    "( ..(..) tkIntLit(2) )"
  check "starColon", "var v*: int\n",
    "var tkSymbol(v)/L tkOpr(*) :(:)/T tkSymbol(int)/L"
  check "strings", "\"a\\nb\" r\"a\\nb\"\n",
    "tkStrLit(a\\nb) tkRStrLit(a\\\\nb)/L"
  check "tripleStr", "\"\"\"a\nb\"\"\"\n",
    "tkTripleStrLit(a\\nb)"
  check "gstr", "re\"x\"\n",
    "tkSymbol(re) tkGStrLit(x)"
  check "chars", "'a' '\\n' '\\x41'\n",
    "tkCharLit(a) tkCharLit(\\n)/L tkCharLit(A)/L"
  check "comment", "a # nope\nb\n",
    "tkSymbol(a) tkSymbol(b)@0"
  check "docComment", "## hi\n## there\nb\n",
    "tkComment(hi\\nthere) tkSymbol(b)@0"
  check "nestedComment", "a #[ x #[ y ]# z ]# b\n",
    "tkSymbol(a) tkSymbol(b)/L"
  check "indent", "a\n  b\n c\n",
    "tkSymbol(a) tkSymbol(b)@2 tkSymbol(c)@1"
  check "unaryMinus", "a - 1 (-1) f(-1)\n",
    "tkSymbol(a) tkOpr(-)/LT tkIntLit(1)/L (/L tkIntLit(-1) ) tkSymbol(f)/L ( tkIntLit(-1) )"
  check "operators", "a += b; c ->> d\n",
    "tkSymbol(a) tkOpr(+=)/LT tkSymbol(b)/L ; tkSymbol(c)/L tkOpr(->>)/LT tkSymbol(d)/L"
  check "accent", "`+`\n",
    "` tkOpr(+) `"
  check "unicodeOpr", "a ∙ b\n",
    "tkSymbol(a) tkOpr(∙)/LT tkSymbol(b)/L"
  check "underscore", "_ = 3\n",
    "tkSymbol(_) =(=)/LT tkIntLit(3)/L"
  if failures == 0: echo "all lexer cases ok"
  else: quit "failures: " & $failures

main()
