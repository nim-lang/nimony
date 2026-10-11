# `parseStmt` and `parseExpr` in a macro.
import std / [syncio, macros]

macro fromText(): untyped =
  result = parseStmt("""
proc triple(x: int): int = x * 3
echo triple(4)
""")

macro exprFromText(): untyped =
  result = parseExpr("triple(10) + 3")

fromText()
echo exprFromText()
