# A `const` whose evaluation raises (here: the file does not exist) is an
# error naming the expression, not a failure to read the evaluator's output.
import std/[syncio, compilation]

const d = staticRead("nosuchfile.txt")
echo d.len
