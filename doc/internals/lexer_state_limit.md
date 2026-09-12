# The `lex` plugin's 255-state ceiling

`std/regex`'s `lex` refuses a pattern set whose automaton needs more than
`MaxLabel = 255` states. This note records what that costs, what the real
constraint turned out to be, and what has to change. It exists because
`src/nifler2/nimlexer.nim` ran into the ceiling immediately and had to be
written around it.

## What the ceiling actually caps

`MaxLabel` bounds *three* automata, and the one that overflows is not the one
that ends up in your program:

1. the NFA built by Thompson's construction (`regExprToNfa`),
2. the DFA from the subset construction (`nfaToDfa`),
3. the minimized DFA from Hopcroft's algorithm (`optimizeDfa`), which is what
   is emitted.

Thompson's construction is deliberately wasteful -- two states per character
class, more per alternation and per repetition -- so the NFA is roughly an
order of magnitude larger than the DFA it produces. Every measurement below
was taken by temporarily raising `MaxLabel` to 3000 and printing
`dfa.stateCount` from the plugin:

| pattern set | minimized DFA | fits in 255 today? |
| --- | --- | --- |
| the 6 numeric-literal patterns, each with its `'suffix` group | **30** | **no** |
| Nim's 66 keywords as literals + identifier + whitespace + `.` | **233** | **no** |
| the same keywords style-insensitively + identifier + whitespace + `.` | **401** | no |
| all of `nimlexer` in one automaton (keywords, identifiers, every numeric form, operators, punctuation) | **432** | no |

The first row is the striking one. Six number patterns compile to a
**30-state** automaton and are rejected anyway, because their NFA passes 255
on the way there. `nimlexer` had to move the `'i8` / `u32` / `'myLit` suffix
out of the patterns and into a hand-written second stage for that reason
alone -- and the suffix scanner it now carries is code the generator was
supposed to make unnecessary.

The second row says the intended design -- "all keywords plus identifiers in
a single automaton" -- would already fit in a 255-state *DFA*. It is only the
intermediate that does not.

## The ceiling is not the only problem

With `MaxLabel` raised to 3000, the full 432-state `nimlexer` automaton builds
and runs correctly: nothing in the emitter assumes a small state count. But it
takes **35 seconds**, and the keyword-only set (401 states) takes 21. That is
per `lex` call, on every compilation that is not cached. Three quadratic terms
account for it:

- `searchInStates` is a linear scan that compares whole state *sets*. The
  subset construction calls it once per (state, alphabet letter) pair, so the
  cost is `states² × 262` set comparisons. This is the dominant term.
- `closure`, `getDfaEdge`, `getPreds`, `card` and `choose` all walk
  `0 .. stateCount` and test membership, rather than iterating the set's
  members. At 255 states that is invisible; at 432 it is not.
- `optimizeDfa` calls `getPreds` -- itself a full walk over all states and
  their transition lists -- once per (partition, letter).

So "raise `MaxLabel`" alone would trade a compile error for a 35-second
compile.

## What has to change

`Label = range[0..MaxLabel]` and `LabelSet = set[Label]` are load-bearing:
`Nfa.trans`, `Dfa.trans` and `toRules` are `array[Label, …]`, and `lab()`
exists only to satisfy the range checker. The change is:

- `Label` becomes a plain `int32`, and `lab()` disappears with it.
- `trans` and `toRules` become `seq`s grown as states are added.
- `LabelSet` becomes a sparse set. **`std/packedsets`, not `std/intsets`**:
  both are `Table[uint, Trunk]`-backed and cost the same, but `intsets` has
  only `incl` / `excl` / `contains` / `containsOrIncl` / `items`, while
  `regexcore` needs `==`, `<=`, `*`, `+`, `-` and `card`, which
  `PackedSet[A: Ordinal]` already has. (Adding the algebra to `intsets`
  instead is the other option and would suit callers who want the non-generic
  spelling.)
- Every `for i in 0 .. stateCount: if lab(i) in s` loop iterates the set
  instead. This is a speedup at any size, and it is what makes a sparse set
  affordable at all -- a Table-backed membership test is much more expensive
  than a bit test in a 32-byte set, so the algorithms must stop asking.
- `searchInStates` gets a hash table from `LabelSet` to state index. The
  subset construction is the only caller that runs it in a hot loop;
  `optimizeDfa`'s use is over the small worklist and can stay linear.

## The trap: iteration order

`set[Label]` iterates in state order. A `Table`-backed sparse set does not,
and two places depend on the order:

- `allDests` fixes the order of the `elif` chain the plugin emits for a
  state's outgoing edges.
- `choose` picks the representative of a partition in `optimizeDfa`, and the
  representative's transitions are the ones that get emitted.

Both must sort, or take the minimum, or the generated parser stops being
reproducible from one compilation to the next. That is a silent failure --
the code still works -- so it needs a test that builds the same pattern set
twice and compares the emitted trees.

## The other reason keywords cannot be literals

Independently of any state limit: Nim's keywords are style-insensitive.
`p_roc` and `pRoC` are the keyword `proc`, while `Proc` is an ordinary
identifier, because identifier equality keeps the first character as written
and lowercases the rest with underscores removed. A keyword is therefore not
a string but a family of them, and `of "proc":` in a `lex` would be wrong
rather than merely limited.

It *is* expressible -- `proc` becomes `p(_?[rR])(_?[oO])(_?[cC])` -- and that
is the 401-state row in the table above. Whether nifler2 should pay 401 states
for it, or keep `nimlexer`'s `nimIdentNormalize` + binary search (which is
what Nim's own identifier cache does), or drop style-insensitive keywords from
the language, is a language decision rather than a plugin one.

## A smaller fix worth doing anyway

The diagnostic says only

    these `lex` patterns need more than 255 automaton states; split them up

It does not say which of the three automata overflowed, and the answer
(almost always the NFA) changes what a user should do about it. Reporting the
stage, and the state count reached, would have made the table above a
one-command answer instead of an afternoon.
