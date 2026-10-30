# Regex execution and search

The literal-prescan commit (`e91f0eb7`) added patchwork to the search layer,
not to an inherently unsound automaton design. Thompson construction, subset
construction, minimization, and compile-time Unicode-to-byte lowering remain
useful. Repeated attempts, AST-derived reverse windows, leading-loop skips,
and heuristic rejection of unanchored patterns were interacting ways of
working around the same search problem.

Regular matching now uses a flat `seq[int32]` transition matrix and a 256-byte
byte-to-column map. A cell holds the next state; its complement additionally
records acceptance at the current position. State zero stops. EOF has its own
column. Acceptance need not be recorded on a transition to another accepting
state, which will supersede it. There is no input decoder or validation.

The common compiler path is AST -> NFA -> minimized DFA -> table. Assertions
are partially evaluated from the existing ordered instruction representation
into tables, preserving historical assertion priority and attempt-relative
`^`/word-boundary semantics. Only the initial row sees the initial-position
context; consumed-byte transitions enter ordinary rows. That distinction is
what makes state equality sufficient for merging search attempts. The
instruction representation is discarded after folding. Backreferences and
zero-width assertion cycles retain the VM; capture replay also retains it.

Compilation parses once, and NFA construction can omit capture markers from
the same AST. Character classes no longer need a separate epsilon entry
state. Adjacent columns made equivalent by minimization are coalesced:
Unicode expansion leaves `.+` with three columns including EOF, rather than
sixteen. Unicode and legacy single-byte alternatives remain the same byte
language; no UTF-8 validation, sanitization, normalization, or decoding is
performed on the subject.

Search advances an ordered frontier of `(state, earliestStart)` pairs. At
one input position, equal states have equal futures, so the earliest start
dominates later arrivals. A per-state generation stamp deduplicates each next
frontier. Once a match is recorded, only starts no later than it remain
eligible; execution continues to find the longest match at the earliest start.
This costs O(subject length * DFA states) time and O(DFA states) scratch space,
including patterns without a required literal. One scalar attempt before the
frontier avoids allocating it on a successful first candidate and adds at most
one linear scan. First-byte skipping and a required-literal absence prefilter
remain; neither defines search correctness. Literal windows, backward prefix
walks, leading-loop metadata, and the fails-fast rejection pass are gone.
`reUnanchored` and `requireAnchor` are compatibility no-ops.

## Measurements

Compared with `e91f0eb7`, using `bench/rex_arch.nim`, C backend, `-d:release`,
checks enabled, default allocator/MM, Linux x86-64, Intel Core Ultra 5 225H,
GCC 16.2.1. Both versions use the same inputs and iteration counts. Cachegrind
3.27.1 instruction totals include construction and setup. Timings are medians
of ten runs after three warmups in one paired hyperfine invocation.

| Workload / define | Before instructions | After instructions | Before median | After median |
| --- | ---: | ---: | ---: | ---: |
| ASCII match / default | 221,431,684 | 115,830,400 | 12.122 ms | 11.389 ms |
| UTF-8 + raw E4/FF match / `archMixed` | 340,719,368 | 160,685,353 | 17.359 ms | 15.190 ms |
| Successful token search / `archSearch` | 221,443,996 | 115,853,328 | 11.912 ms | 11.109 ms |
| Sparse singleton / `archPrefix` | 3,625,013 | 3,626,286 | — | — |
| Absent literal / `archAbsent` | 6,358,141 | 6,286,955 | — | — |
| Late failure, `(?:a|ab)*c` / `archLate` | 1,023,481,600 | 4,106,896 | 45.185 ms | 0.548 ms |
| No literal, `^\d+$` / `archNoLiteral` | 1,845,714,675 | 4,089,611 | 81.164 ms | 0.487 ms |
| 800 compilations / `archCompile` | 280,967,550 | 273,532,600 | 17.156 ms | 16.832 ms |

Sub-millisecond timing results include process startup and are noisy; the
249x and 451x instruction reductions are more useful than timing ratios there.
The compilation timing difference is small; instruction counts show a 2.65%
reduction. Sparse and absent-literal scans are effectively unchanged.

Experiments retained or revised:

- Routing every pattern through bytecode before folding added about 3.7% to
  compilation instructions. Direct lowering for assertion-free patterns
  removed that unnecessary intermediate stage.
- Always starting with a frontier used 534M instructions for successful token
  search versus 221M before. One scalar candidate restores the common path;
  the final version uses 116M, while late failures remain bounded.
- Column coalescing reduces `.+` table/map payload from 448 to 292 bytes and
  `\w+@\w+` from 736 to 496, at a small compilation cost.

Tables trade space for execution speed. Including the byte map but excluding
object/allocator headers, a singleton `z` grows from 32 bytes of VM payload to
304 bytes of table payload; `[a-z0-9]+` grows from 80 to 328 bytes. This is not
an across-the-board memory reduction. Search scratch is bounded by automaton
size, not input length. No memo table or reverse automaton is built.

To reproduce a variant (omit the define for ASCII matching):

```sh
bin/nimony c -d:release -d:archMixed --nimcache:/tmp/rex-mixed-cache \
  --out:/tmp/rex-mixed bench/rex_arch.nim
valgrind --tool=cachegrind --cachegrind-out-file=/tmp/rex-mixed.cg /tmp/rex-mixed
hyperfine --warmup 3 --runs 10 /tmp/rex-before-mixed /tmp/rex-mixed
```

## Verification and limits

`trex_tables.nim` compares table matching and search with the instruction VM:
32 patterns, all 2,801 subjects of lengths zero through four over a seven-byte
alphabet, at every start including beyond EOF, plus longer mixed/malformed
subjects. It also tests compiled literals, capture offsets, leftmost-longest
selection, and formerly rejected patterns. Existing `tregex`, `trex_unicode`,
`tlexim`, `tlexbase`, and `targgenericrematch` pass, as do both regex benchmark
smoke tests. The full compiler suite was not run.

Capture bounds remain best-effort, and backreferences still use repeated VM
attempts. Zero-width assertion cycles retain legacy VM behavior. DFA subset
construction can still grow exponentially. Repeated searches such as
`findAll`/`replace`/`split` can still rescan suffixes when finding a short
successful match required long lookahead; the linear-in-subject bound is per
search, not a guarantee for a complete series of searches.
