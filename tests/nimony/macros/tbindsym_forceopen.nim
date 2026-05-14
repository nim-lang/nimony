# `brForceOpen` differs from `brOpen` only when the def-site lookup finds a
# single symbol: `brOpen` emits a bare nnkSym (sem treats as resolved);
# `brForceOpen` emits a one-element nnkOpenSymChoice so sem at the call site
# can still augment the candidate set. With a unique system symbol like
# `succ`, both forms compile and call the same proc — but their plugin output
# NIF differs.
#
# This test exercises the codepath; the visible difference is in the plugin's
# `macro_out_*.nif`:
#   brOpen      → (call succ.0.sysvq0asl 0)
#   brForceOpen → (call (ochoice succ.0.sysvq0asl) 0)
import std / [syncio, macros]

macro callBrOpen(): untyped =
  result = newCall(bindSym("succ", brOpen), [newIntLitNode(0)])

macro callBrForceOpen(): untyped =
  result = newCall(bindSym("succ", brForceOpen), [newIntLitNode(0)])

echo callBrOpen()
echo callBrForceOpen()
