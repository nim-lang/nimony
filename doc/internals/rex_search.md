# rex search memoization

Based on rex-unicode `55670ce4`. No reverse automaton, threshold tuning,
bytecode changes, Regex fields, or extra compilation work.

## Design

The search-only interpreter uses Reps's failed-configuration memoization.
Each entry PC maps to one of three things: an accepting state, a memo slot for
a nonaccepting state that can reach itself, or nothing. Only cyclic
nonaccepting states get a slot. A state that cannot be revisited is entered at
most once per attempt, so remembering its positions buys no pruning and costs
a bit per input byte; `a{600}b` pays 602 bits per byte for nothing, while
`a+b|a{2000}z` needs one. Cycles are found once per activation with an
iterative Tarjan pass over the state graph that the test/jump pairs already
spell out: O(code length), and no recursion proportional to a bounded
repetition.

A memoized state at position `p` is pending only when `p > start`. Entry into
**every** accepting state clears pending pairs, including states whose
bytecode omits `opcAccept` and ends in `opcRetAccept`. A failed suffix marks
its pairs doomed. Reaching a doomed pair returns the current attempt's last
accept. Marker tests and captures keep their original semantics; `capCode` is
run separately after selection.

The first attempt with an unproductive suffix only enables memoization.
The next attempt builds the PC map and the cycle marks; the bitset is
allocated only when an instrumented attempt actually leaves pending doomed
pairs. There is no ratio threshold. Anchored `exec`/`matchLen` use the
original interpreter specialization; generated lexers do not use this
machinery.

For a fixed DFA, successful prefixes of a non-overlapping traversal are
disjoint. Unproductive pairs are explored once after activation, then later
attempts stop at them. The first unproductive suffix may be explored once
more. Start-position marker chains cost at most O(states) per attempt.
Thus match selection is O(states × input bytes), plus preprocessing of the
bytecode; an acyclic automaton needs no memo at all, because an attempt cannot
outlast the automaton. Memo state is local to one search API invocation or
`findAll` traversal; separate `findBounds` calls do not share it. Capture
extraction retains its separate existing interpreter and is not memoized.

The bitset cap is 8 MiB, one bit per eligible state and position, including
EOF. Eligibility is the whole cost model: a pattern pays per input byte what
its cyclic nonaccepting states are worth, not what its automaton is worth.
Exceeding the cap falls back to the old matcher. Back references also fall
back: state and position do not determine future behavior without captures.
Those fallbacks can remain quadratic. The cap applies to the bitset; the PC
map and pending-suffix sequence consume additional memory.

## Verification

- Baseline and modified stdlib suite: 85/85 passed.
- Final focused `tregex` and `trex_unicode` tests passed, including bytewise
  advancement, invalid UTF-8, continuation-byte starts, captures, replacement,
  splitting, longest/leftmost ordering, and the memory-cap fallback.
- Cycle gate: `tests/nimony/stdlib` 85/85, `tregex` and `trex_unicode` pass,
  and the `bench` smoke suite is 5/5. The differential patterns added to
  `tregex` (`a+b*c+d`, `a+X|b+Y`, `a{64}b`, `(?:a+b?)+X`, …) are checked
  against the per-position matcher, and the cap test now needs over 64
  *eligible* states to fall back.
- Mutation: returning no match on a memo hit instead of the saved accept made
  the differential test fail; restored code passed.
- A 30-pattern dump of bytecode, data, first bytes and DFA transitions/rules,
  plus the three benchLexer rule sets, is byte-for-byte identical.
- Fresh nifler2 build: generated lexer C and outputs for three fixed Nim
  sources are byte-for-byte identical.
- New `benchFailedSearch` and `benchShortSearch` smoke outputs: `0` and `120`.
- The full-suite attempt completed 1109/1117 before the requested scope was
  narrowed. Seven failures involved missing worktree dependencies (mimalloc
  or the pinned ORC runtime), subsequently restored; `examples/io_basics`
  also failed. Full-suite success is not claimed. Earlier sandbox/quota runs
  were discarded. Final verification follows the requested rex/nifler2 scope.

No behavior change was observed. The known attempt-relative `^` behavior
is explicitly tested and preserved.

## Measurements

Intel Core Ultra 5 225H, Linux x86-64; host Nim 2.3.1, Valgrind 3.27.1,
hyperfine 1.20.0. Nimony release builds, bounds checks enabled, fresh nimcache
for each build after regexcore edits. Instruction totals use cachegrind with
cache and branch simulation off. Times are one hyperfine median of five runs
with one warmup, for the whole process. The final measurement pass runs
before/after binaries serially without compilation or tests in parallel.
Sub-millisecond timings include process overhead and are noisy; instruction
counts are the clearer comparison there.

### Cycle-only memoization

Same machine, same harness, `find` on a run of `a` bytes; "before" is the
unspecialized memo (one bit per state), "after" is eligible states only.
Times are wall clock for one call, not instructions.

| Pattern | States | Eligible | Before | After |
| --- | ---: | ---: | ---: | ---: |
| `a{600}b`, n=80 000 | 602 | 0 | 207 ms | 79 ms |
| `a+b&#124;a{2000}z`, n=50 000 | 2 003 | 1 | 1 687 ms | 216 ms |
| `a+b&#124;a{2000}z`, n=100 000 | 2 003 | 1 | 6 629 ms | 406 ms |
| `a+b`, n=80 000 | 3 | 1 | 1 ms | 1 ms |
| `\w+@\w+`, n=80 000 | 4 | 1 | 1 ms | 1 ms |

Two effects. `a{600}b` has no cyclic nonaccepting state, so search now runs
the plain matcher and stops paying for a 5.8 MiB bitset it could not use; a
bare `matchLen` loop over the same subject takes 52 ms, so what is left is the
search helper, not memoization. `a+b|a{2000}z` used to exceed the cap on state
count alone (2 003 states × 100 000 bytes) and stayed quadratic, 4× per
doubling; with one eligible state it is linear in the subject, the remaining
cost being the 2 000-state `a{2000}` chain every attempt still has to walk.

### Doubling

Each subject is a run of `a` bytes; the `$` case appends `!`. Each invocation
iterates `findAll` once, so the short-success case exercises repeated search.
Counts include input construction and process startup.

| Pattern | Before 2k / 4k / 8k instructions | After 2k / 4k / 8k instructions | After 4k→8k |
| --- | --- | --- | --- |
| `\w+@\w+` | 136,661,688 / 545,172,505 / 2,178,193,732 | 1,284,522 / 2,418,289 / 4,730,060 | 1.96× |
| `a&#124;a*b` | 58,692,751 / 233,234,568 / 930,317,795 | 1,406,601 / 2,662,368 / 5,218,139 | 1.96× |
| `(?:a&#124;ab)*c` | 58,704,707 / 233,258,524 / 930,365,751 | 1,094,646 / 2,038,399 / 3,969,198 | 1.95× |
| `.*x` | 92,683,707 / 369,216,524 / 1,474,281,751 | 1,180,250 / 2,210,223 / 4,310,150 | 1.95× |
| `a+$` | 58,694,671 / 233,238,488 / 930,325,715 | 1,114,501 / 2,078,254 / 4,049,053 | 1.95× |
| `a+\bX` | 158,658,625 / 633,166,442 / 2,530,181,669 | 1,320,479 / 2,490,246 / 4,874,017 | 1.96× |

### Full workload table

Reni's pinned `68c8e0d5dcc44f0ef9ef36c9d0f8ea182ab67777` workloads use
1,000 cyclic code lines and enough cyclic Japanese/ASCII text lines to reach
the same size. This compares rex before/after, not rex against reni. Literal
quote characters are escaped for rex; `{k,}` is expressed as `{k}` followed
by `*`. Unsupported cases were not timed: case-insensitive flags, POSIX
classes, Unicode properties (including Han), lazy/possessive semantics,
named captures, and the three lookaround workloads. All other 19 workload
items are represented. Word classes, boundaries, anchors, dot/newline,
alternation and captures retain rex semantics, which differ from Oniguruma.
Ordinary matching uses 1,000 repetitions of two `#define` lines containing
hex values plus a nonmatching line.

| Workload | Before instructions | After instructions | Before ms | After ms |
| --- | ---: | ---: | ---: | ---: |
| default | 3,685,055,765 | 3,685,105,765 | 124.953 | 115.389 |
| benchLongCapture | 538,135,434 | 538,185,434 | 21.248 | 24.722 |
| benchSearch | 13,307,261 | 13,519,904 | 0.739 | 1.094 |
| benchUnicodeSearch | 13,370,193 | 13,582,822 | 1.035 | 0.922 |
| benchUnicodeWide | 13,271,358 | 13,484,001 | 0.802 | 0.722 |
| benchCompile | 34,330,855 | 34,332,955 | 2.573 | 2.946 |
| benchParse | 278,185,176 | 278,185,162 | 19.092 | 18.751 |
| benchGenerate | 162,109,505 | 162,109,519 | 9.851 | 11.689 |
| benchLexer | 515,143,790 | 515,143,776 | 35.222 | 33.239 |
| benchCaptures | 337,076,290 | 337,086,802 | 18.444 | 17.702 |
| literal/common | 1,263,143 | 1,879,092 | 0.281 | 0.333 |
| literal/rare | 818,143 | 1,091,334 | 0.328 | 0.439 |
| literal/UTF-8 | 858,255 | 1,144,673 | 0.333 | 0.420 |
| class/digits | 1,713,746 | 1,820,948 | 0.787 | 0.620 |
| class/word | 4,699,151 | 5,229,253 | 0.620 | 0.942 |
| class/negated | 4,881,746 | 5,357,648 | 1.174 | 0.644 |
| alternation/small | 18,256,737 | 24,728,546 | 1.151 | 1.812 |
| quantifier/greedy | 3,203,886 | 3,146,873 | 0.594 | 0.567 |
| quantifier/bounded | 13,815,379 | 20,908,851 | 1.412 | 1.743 |
| anchor/start | 21,654,149 | 29,309,892 | 1.491 | 1.936 |
| anchor/boundary | 13,326,779 | 18,740,453 | 1.578 | 1.384 |
| capture/two | 21,983,966 | 17,478,483 | 2.067 | 1.188 |
| capture/backref | 38,821,833 | 40,300,530 | 2.544 | 2.571 |
| Unicode/word | 3,965,298 | 4,164,993 | 0.474 | 0.723 |
| no-match/literal | 642,953 | 712,491 | 0.379 | 0.396 |
| no-match/class | 1,946,017 | 2,586,415 | 0.381 | 0.507 |
| assignment-LHS | 16,956,349 | 15,227,889 | 1.724 | 1.133 |
| repeated-group | 16,528,349 | 16,896,348 | 1.670 | 1.268 |
| hex on code corpus | 833,455 | 1,045,920 | 0.504 | 0.336 |
| #define on code corpus | 691,723 | 804,699 | 0.329 | 0.273 |
| alternation/large | 28,490,094 | 40,115,588 | 1.459 | 2.917 |
| ordinary hex matches | 1,191,165 | 1,296,274 | 0.527 | 0.244 |
| ordinary #define matches | 1,112,165 | 1,238,274 | 0.409 | 0.358 |
| `\w+@\w+` (8k) | 2,178,193,732 | 4,730,060 | 96.151 | 0.543 |
| `a&#124;a*b` (8k) | 930,317,795 | 5,218,139 | 56.130 | 0.473 |
| `(?:a&#124;ab)*c` (8k) | 930,365,751 | 3,969,198 | 54.220 | 0.468 |
| `.*x` (8k) | 1,474,281,751 | 4,310,150 | 80.950 | 0.601 |
| `a+$` (8k) | 930,325,715 | 4,049,053 | 51.639 | 0.467 |
| `a+\bX` (8k) | 2,530,181,669 | 4,874,017 | 106.142 | 0.476 |

Ordinary-pattern regressions were investigated using the instruction counts
and a second implementation measurement. Tracking every attempt initially
cost up to roughly 2× on these corpus items. Delaying activation removed much
of the cost for accepting loops. Remaining costs come from the common search
helper and, once a failed prefix activates memoization, state lookup, pending
pair recording and bitset handling. Sparse literals and bounded repetitions
still regress; this change is not an across-the-board throughput improvement.
Regex compile/parse/generation counts remain effectively unchanged, and no
work was added to regexToDfa, rulesToDfa, genBytecode or the compile-time plugin.

Raw logs, dump harness, scaling harness and JSON measurements are in the
session directory `/tmp/rex-session-20261029`; the large full-suite scratch
cache used `nimcache_static/rex-session-20261029` in the main workspace.

## Sources

- [Reps, Maximal-munch tokenization in linear time](https://ftp.cs.wisc.edu/pub/techreports/1997/TR1347.pdf),
  especially Figure 1(b) and its argument. Algorithm implemented here;
  no source code copied.
- [RE2 dfa.cc](https://github.com/google/re2/blob/main/re2/dfa.cc) and
  [re2.cc](https://github.com/google/re2/blob/main/re2/re2.cc): longest-match
  state ordering and forward/reverse search. No reverse machinery reused.
- [regex-automata documentation](https://docs.rs/regex-automata/latest/regex_automata/):
  lazy DFA and iteration's quadratic bound. Its earliest-match option changes
  greedy semantics and therefore is not suitable here.
- Local reni at the pinned commit above: MIT, Copyright (c) 2026 Shuu.
  Benchmark corpus lines used only in the scratch harness with attribution
  and its license retained. Its backtracking and rune advancement semantics
  were not adopted.
