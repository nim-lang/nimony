# Standard Library Guide

This guide covers the non-obvious parts of Nimony's standard library —
behavioral contracts, performance guidance, and design decisions that
aren't apparent from reading the type signatures alone.

For complete API details, read the source directly. For usage patterns,
see the [examples](../examples/) directory (run them with `hastur examples`).


## system

[Source](../lib/std/system.nim)

The system module is always imported implicitly.

### Strings and sequences

`string` and `seq` use value semantics. Assignment copies; strings
are copy-on-write internally.

- `beginStore` / `endStore`: The safe way to do bulk writes into a
  string through a pointer. `beginStore(s, len, start)` returns a
  `ptr UncheckedArray[char]` pointing at position `start`; you may
  write up to `len` bytes through it. You **must** call `endStore(s)`
  when done.

  Why `endStore` is required: Nim strings use Small String Optimization.
  Long strings store their data on the heap, but the first bytes are
  **cached inline** in the string object for fast access. After a bulk
  write through the heap pointer, this inline cache is stale.
  `endStore` syncs it back. Forgetting `endStore` means subsequent
  reads via normal `s[i]` return wrong data for the first characters.

  `beginStore` also handles copy-on-write (ensures the heap buffer is
  unique before returning a mutable pointer).

  ```nim
  var s = newString(100)
  let p = beginStore(s, 5)  # get pointer, ensure unique
  p[0] = 'h'
  p[1] = 'e'
  p[2] = 'l'
  p[3] = 'l'
  p[4] = 'o'
  endStore(s)               # sync inline cache — do not forget!
  ```

  The older `prepareMutation` only handles copy-on-write but does
  **not** sync the inline cache, so it is insufficient with SSO.
  Prefer `beginStore`/`endStore`.
- `del`: Removes an element from a `seq` by swapping it with the last
  element. O(1) but **changes order**. If you need stable order, shift
  elements manually.
- `grow` / `shrink` are preferred over `setLen` — they are more
  explicit about direction. `grow` takes a target length and a value
  to initialize new elements with: `s.grow(newLen, val)`.
  `growUnsafe` skips initialization; use with care.

### Comparison and equality

- Implement only `==`, `<=` and `<` for your types. `!=`, `>`, `>=` are
  generic templates that derive from those two.
- `is` / `isnot` are compile-time type checks. `of` is a runtime
  subtype check (for object inheritance).
- `cmp` returns `int` (negative, zero, positive) — use it for sorting,
  not for equality testing.

### Memory management

- `swap` exchanges bits directly. It does **not** call `=sink`,
  `=copy`, or `=dup` hooks. This is by design — it's faster this way and Nim's lifetime tracking hooks do not support "self-pointers" (pointers inside the same object, `obj.field = addr(obj)`).
- `allocFixed` / `deallocFixed`: Low-level fixed-size allocation.
- `setOomHandler`: By default the runtime tries to continue after
  out-of-memory. For many applications, just quitting is the more robust solution — set a handler that calls `quit`.

#### Strategies (`--mm:`)

`--mm:NAME` selects the strategy. `system.nim` says `include "$MM"` and the
compiler expands `$MM` to `system/<name>`, lowercased — so `--mm:atomicArc`
includes `lib/std/system/atomicarc.nim`. Adding a strategy is adding a file
under `lib/std/system/`; nothing in `system.nim` enumerates them.

| `--mm:` | file | notes |
| --- | --- | --- |
| `atomicArc` | `system/atomicarc.nim` | default; the reference count is updated atomically, so a `ref` may be shared between threads |
| `arc` | `system/arc.nim` | the same, minus the atomics: cheaper counters, but a `ref` must stay on one thread |

A strategy module defines exactly three primitives — `arcInc`, `arcDec` (true
when the count reached zero) and `arcIsUnique`. What is built on top of them
(`GC_ref` / `GC_unref`) is strategy independent and lives in `system/refops`.

`--mm:NAME` also defines `gcName`, so `--mm:arc` makes `defined(gcArc)` true and
`--mm:atomicArc` makes `defined(gcAtomicArc)` true. Switching strategies changes
the cached build configuration and therefore forces a rebuild.

#### The uniquely-referenced fast path

`atomicArc`'s `arcDec` reads the count first and skips the atomic
read-modify-write when it is already zero. A zero count means the caller holds
the only reference, so there is nothing to adjudicate against another thread and
nothing to write back into a cell that is about to be freed. See the comment on
`arcDec` for why that is sound, and why a strategy that ever gains a collector
must not inherit it.

That makes the caller's contract load-bearing: **free the cell when `arcDec`
returns true, or already know the count is non-zero.** A caller that discards a
`true` leaves the count untouched rather than at -1.

On `bench/gcbench.nim` (`-d:danger`, median of 31 pinned runs) the fast path is
worth -36%, which puts atomicArc level with non-atomic `arc`:

| | |
| --- | --- |
| `--mm:atomicArc`, `-d:nimNoAtomicArcFastPath` | 0.1334 s |
| `--mm:atomicArc` | 0.0847 s |
| `--mm:arc` | 0.0881 s |

The worst case -- a decRef that always sees a non-zero count, so the load never
pays off -- measures +0.1%. `-d:nimNoAtomicArcFastPath` restores the previous
code path. The same trick does *not* belong in `arc`: measured there it is a
2--4% loss, because a non-atomic store is cheaper than the branch that avoids it.

### Iterators

- `..` is inclusive: `0..3` yields 0, 1, 2, 3.
- `..<` is exclusive on the right: `0..<3` yields 0, 1, 2.

### Coroutines

The system module defines `Continuation`, `ContinuationProc`,
`CoroutineBase`, and `Scheduler` for continuation-based concurrency.
See the CPS documentation for details.

[Examples: seqs and strings](../examples/seqs_and_strings.nim)


## syncio

[Source](../lib/std/syncio.nim)

Synchronous file I/O. `echo`, `stdin`, `stdout`, `stderr` are always
available.

- `open` returns bool**, not a File — check it. The `File` is an
  out-parameter.
- `readLine` returns `false` at EOF. Use it in a `while` loop.
- `writeLine` appends the platform line ending.
- `tryWriteFile` is a convenience that opens, writes, and closes
  in one call.
- `quit(msg)` writes to stderr and exits — useful for fatal errors.

[Examples: file I/O](../examples/io_basics.nim)


## strutils

[Source](../lib/std/strutils.nim) ·
[Examples](../examples/strutils_basics.nim)

ASCII string operations. For Unicode, use `unicode` instead.

### Character sets

The module exports `const` sets: `Whitespace`, `Letters`, `Digits`,
`HexDigits`, `IdentChars`, `IdentStartChars`, `Newlines`,
`PrintableChars`, `AllChars`, and subsets like `UppercaseLetters` /
`LowercaseLetters` / `PunctuationChars`.

Use `AllChars - ValidChars` to build an *invalid* character set for
`find`.

### Comparison

- `cmpIgnoreCase` returns `int` (not `bool`) — designed for sort
  comparators.
- `cmpIgnoreStyle` ignores case **and** underscores, matching
  Nim's identifier comparison rules: `foo_bar` == `FooBar`.
- `normalize` lowercases and strips underscores — the canonical
  form for Nim-style comparison.

### Splitting

`split` with `set[char]` treats each character as an independent
separator. Adjacent separators produce empty strings in the output.
Use `maxsplit` to limit the number of splits.

### Search and replace

- `find` returns `-1` on no match, not an exception.
- `replace` returns a new string; it does not mutate.
- `replaceWord` only matches whole words (checks word boundaries).
- `multiReplace` applies multiple replacements in a single pass.

### Formatting

- `%` interpolates `$1`, `$2`, ... placeholders. `format` is the
  same thing as a named proc.
- `formatFloat` / `formatBiggestFloat`: Use `ffDecimal` for
  fixed decimals, `ffScientific` for scientific notation.
- `formatSize` formats byte counts as human-readable strings
  (KiB, MiB, etc.).


## unicode

[Source](../lib/std/unicode.nim)

Full Unicode support. Operates on `Rune` (a single code point).

Key distinction: **byte position vs rune position**. Most procs
take byte positions for efficiency. Use `runeOffset` to convert
a rune index to a byte index.

- `runeLen` returns the number of runes (not bytes).
  `s.len` gives bytes.
- `fastRuneAt` is a template that avoids allocation — use it
  in tight loops instead of `runeAt`.
- `graphemeLen` accounts for combining characters — one
  "visible character" may be multiple runes.
- `toLower` / `toUpper` here handle the full Unicode range,
  unlike `strutils.toLowerAscii`.
- `validateUtf8` returns -1 if valid, otherwise the byte
  position of the first invalid byte.


## hashes

[Source](../lib/std/hashes.nim)

Hash values are prerequisites for `Table` and `HashSet` keys.

- Combine hashes with `!&`, finalize with `!$`.
- The `Hashable` concept describes what a type needs to be hashable.
- `hashIgnoreStyle` matches `cmpIgnoreStyle` — use them together
  for Nim-style identifier tables.


## tables

[Source](../lib/std/tables.nim) ·
[Examples](../examples/tables_basics.nim)

Generic hash tables. Keys must satisfy `Keyable` (have `==` and `hash`).
You must `import std/hashes` alongside `std/tables` to make the `hash`
procs visible — tables does not re-export them.

The table uses a hybrid strategy: **linear scan for ≤ 4 entries**,
hash table with open addressing for larger sizes. This means small
tables have no hashing overhead at all.

- `[]` raises `KeyError` on missing keys. Use `getOrDefault` for
  a safe fallback, or `contains` to check first.
- `mgetOrPut` returns a `var` reference — ideal for counters:
  `tab.mgetOrPut(key, 0) += 1`.
- `getOrQuit` is for cases where a missing key is a programming
  error — it calls `quit`, not an exception.


## sets (HashSet)

[Source](../lib/std/sets.nim) ·
[Examples](../examples/sets_basics.nim)

Hash-based sets for any hashable type. Built on top of `Table[T, bool]`.
Like `tables`, you must also `import std/hashes`.

- `containsOrIncl` is the deduplication primitive — returns whether
  the element was already present, and adds it if not.

For small ordinal types (`char`, `enum`, small integer ranges), prefer
the built-in `set[T]` which is a bitset and much faster.


## paths

[Source](../lib/std/paths.nim)

Type-safe path handling via the `Path` distinct type.

- `/` joins paths and normalizes. Use it instead of string
  concatenation.
- `splitFile` returns `(dir, name, ext)` where `ext` includes the
  leading dot. Empty components are empty strings, never `/`.
- `relativePath` can use `'/'` as separator (via `sep` parameter)
  for URL construction.
- `absolutePath` defaults to the current directory as root.
- `expandTilde` handles `~/` expansion. Works on all platforms
  including Windows.


## dirs

[Source](../lib/std/dirs.nim)

Directory operations.

- `createDir` creates nested directories (like `mkdir -p`).
  No error if it already exists.
- `walkDir` yields `(kind, path)` tuples. The `kind` distinguishes
  files, directories, and symlinks. Set `relative = true` to get just
  names instead of full paths.
- `tryRemoveFinalDir` / `tryRemoveFile` return OS error codes
  instead of raising — useful when you need to distinguish "not found"
  from "permission denied".


## envvars

[Source](../lib/std/envvars.nim)

- `getEnv` returns `""` for missing variables. Use `existsEnv` to
  distinguish missing from empty.
- `envPairs` iterates all environment variables as `(key, value)`.


## appdirs

[Source](../lib/std/appdirs.nim)

Platform-correct application directories (XDG on Linux, `Library/` on
macOS, `AppData` on Windows).

- `getConfigDir` / `getDataDir` / `getCacheDir` respect
  `XDG_*` environment variables on Unix.
- `getTempDir` does **not** verify the directory exists.


## memfiles

[Source](../lib/std/memfiles.nim)

Memory-mapped file I/O. The `MemFile` type exposes `mem` (pointer)
and `size`.

- `open` with `mappedSize = -1` maps the entire file.
- The `offset` parameter must be a multiple of the OS page size.
- `close` flushes changes for files opened with write access.


## encodings

[Source](../lib/std/encodings.nim)

Character encoding conversion. Uses iconv on Unix, Windows API on
Windows.

- `getCurrentEncoding` always returns `"UTF-8"` on Unix.
- Open a converter with `open(srcEncoding, destEncoding)`, then call
  `convert`. Don't forget to `close` it.


## wordwrap

[Source](../lib/std/wordwrap.nim)

- `wrapWords` handles Unicode graphemes correctly.
- Set `splitLongWords = false` to keep long words intact instead of
  breaking them mid-word.


## rawthreads

[Source](../lib/std/rawthreads.nim)

Low-level thread creation.

- `create` takes a `fn(arg)` pair and an optional stack size
  (0 = system default, typically 2MB).
- `pinnedToCpu` is best-effort — some platforms (macOS) do not
  support CPU pinning.
- Always `join` threads before the program exits.


## stacktraces

[Source](../lib/std/stacktraces.nim)

`getStackTrace()` returns the call stack as one proc name per line,
innermost frame first. `getStackTrace` itself is never listed; pass
`skip = n` to drop that many further frames, which is what a panic
handler wants so its own frame does not head the trace.

- **Native backend only.** `stackTracesAvailable` is a `const`: true for
  `nimony n` on amd64, false everywhere else, where `getStackTrace`
  returns `""`. On the C backend the frames belong to the C compiler and
  are described by its unwind tables; reading those is a different
  implementation rather than a missing branch of this one. Branch on the
  const rather than on the result, so the difference is visible at
  compile time.
- **Names, not source positions.** There is no line-number table yet, and
  the disambiguator and module suffix a NIF symbol carries
  (`leaf.3.stagd0hts`) are dropped — the frame *sequence* is what
  distinguishes two procs of the same name.
- **Inlined frames do not appear.** A proc the inliner spliced into its
  caller has no frame to find, so the caller is what the trace names.
  Mark a proc `{.noinline.}` if you need to see it.
- It allocates the result string like any other proc; there is no
  allocation-free variant yet.

How it works — nifasm lays a per-proc table (code range, frame size, name)
at the end of `.text`, and the walk is a binary search per frame. See
`nativenif/doc/tracetable.md`.
