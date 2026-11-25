# A literal with a suffix, written `(suf LIT "SFX")` in NIF, reaches a macro
# as the literal of the matching kind, and a macro returns it with its type.
import std / [syncio, macros]

macro describe(s: untyped): untyped =
  case s.kind
  of nnkStrLit..nnkTripleStrLit: newStrLitNode($s.kind & " " & s.strVal)
  of nnkIntLit..nnkUInt64Lit: newStrLitNode($s.kind & " " & $s.intVal)
  else: newStrLitNode($s.kind & " " & $s.floatVal)

echo describe(r"raw\n")
echo describe("""triple""")
echo describe(-8'i8)
echo describe(300'u16)
echo describe(2.5'f32)

macro rawArg(s: untyped): untyped = newStrLitNode($s[1].kind & " " & s[1].strVal)
echo rawArg(foo"x\y")

macro bump(s: untyped): untyped =
  result = s
  result.intVal = s.intVal + 1
proc k(x: int8): string = "int8 " & $x
proc k(x: int64): string = "int64 " & $x
echo k(bump(1'i8))
