# nim-lang/nimony#1626: literal-only float expressions constant-fold in
# hexer's desugar pass using maximum (float64) precision, so mixed-precision
# comparisons over literals agree with `const` evaluation.
import std/syncio

echo 0.09'f32 + 0.01'f32 == 0.09'f64 + 0.01'f64   # folds to true

# const and runtime contexts must agree:
const x = 0.09'f32 + 0.01'f32 == 0.09'f64 + 0.01'f64
echo x

# a non-constant operand blocks folding; runtime f32 arithmetic keeps
# standard IEEE behavior:
var a = 0.09'f32
echo a + 0.01'f32 == 0.09'f64 + 0.01'f64          # false

# a folded arithmetic result keeps its f32 static type:
let d = 0.09'f32 + 0.01'f32
echo d

# operands the evaluator cannot reduce leave the expression alone rather
# than reaching for the SemContext that hexer does not have:
type Obj = object
  a, b: int
echo float(sizeof(Obj)) + 1.0
echo float(1 shl 4) + 0.5
echo float(not 0'i32) + 0.5
proc f(): float = 2.0
echo f() + 1.0
