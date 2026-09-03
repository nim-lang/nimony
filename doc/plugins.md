# Plugins

Plugins are Nimony's metaprogramming mechanism, replacing Nim 2's macro system.
A plugin is a separate Nim program that transforms NIF (Nimony Intermediate Format)
trees at compile time. Plugins run as external processes, communicating with the
compiler through NIF files.

## Overview

There are five kinds of plugins:

| Kind | Declaration | Scope |
|------|-------------|-------|
| **Template plugin** | `template foo(...) {.plugin: "path".}` | Invoked at each call site |
| **For-loop plugin** | `iterator foo(...) {.plugin: "path".}` | Rewrites a `for` loop using the iterator |
| **Module plugin** | `{.plugin: "path".}` as statement | Transforms the entire module |
| **Type plugin** | `type T {.plugin: "path".} = ...` | Invoked for every module that uses `T` |
| **Import plugin** | `import (path/foo) {.plugin: "v2".}` | Imports the module `path/foo` **from the plugin** `v2` |

The first four share the same API (`plugins`) and execution model. Import
plugins are different in both respects; see [Import plugins](#import-plugins).


## Quick start

A plugin replaces a bodiless template. Where you would write:

```nim
template generateEcho(s: string) = echo s
```

You instead write:

```nim
# mymodule.nim
import std / syncio

template generateEcho(s: string) {.plugin: "deps/mplugin1".}

generateEcho("Hello, world!")  # prints: Hello, world!
```

And in `deps/mplugin1.nim`:

```nim
import plugins

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  var arg = callArgs(n)
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.takeTree arg

var inp = loadPluginInput()
saveTree transform(inp)
```

The plugin reads the template arguments as a NIF tree from `paramStr(1)`, transforms
them, and writes the result to `paramStr(2)`.


## NIF enum classes

NIF identifies every compound node by a textual tag (`call`, `proc`, `object`, ...).
For dispatching on nodes, plugins don't match on the raw text — they use one of five
enum classes that partition tags by syntactic role:

| Enum | Suffix | Sentinel | Role |
|------|--------|----------|------|
| `NimonyStmt` | `S` | `NoStmt` | Statements (`CallS`, `VarS`, `ProcS`, `StmtsS`, `BlockS`, ...) |
| `NimonyExpr` | `X` | `NoExpr` | Expressions (`CallX`, `DotX`, `AddX`, `TrueX`, `NilX`, ...) |
| `NimonyType` | `T` | `NoType` | Types (`ObjectT`, `ArrayT`, `IntT`, `RefT`, ...) |
| `NimonyPragma` | `P` | `NoPragma` | Pragmas (`InlineP`, `MagicP`, `PluginP`, ...) |
| `NimonyOther` | `U` | `NoSub` | Substructures that aren't stmts/exprs/types/pragmas (`ParamU`, `FldU`, `ElifU`, `OfU`, `StmtsU`, `PragmasU`, ...) |

All five enums share one ordinal space (each value's ordinal equals the underlying
`TagId`), so dispatch is an O(1) integer comparison and the enums are trivially
convertible to `TagId`.

The full list of tags and which class each belongs to can be found in
[tags.md](tags.md).

### Querying the current node

Each class has a matching accessor on `NifCursor`:

| Accessor | Returns | When it returns the sentinel |
|----------|---------|------------------------------|
| `n.stmtKind` | `NimonyStmt` | current token isn't a statement tag |
| `n.exprKind` | `NimonyExpr` | current token isn't an expression tag |
| `n.typeKind` | `NimonyType` | current token isn't a type tag |
| `n.pragmaKind` | `NimonyPragma` | current token isn't a pragma tag |
| `n.otherKind` | `NimonyOther` | current token isn't a substructure tag |

A typical dispatch pattern:

```nim
case n.stmtKind
of CallS:
  # handle a call statement
of VarS, LetS:
  # handle a variable declaration
of NoStmt:
  # not a statement — try another class, or just copy the token through
  result.takeTree n
else:
  result.takeTree n
```

### Same text, different class

A handful of tags appear in more than one class because the same syntax serves
different roles in different positions:

- `nil` is `NilX` as an expression (a nil pointer value) and `NilU` as an annotation
  on a pointer type.
- `kv` is `KvX` as a key-value expression and `KvU` as a structural field.
- `and`, `or`, `not` are `NimonyExpr` values in expressions and `NimonyType` values
  inside concept constraints.

If you need to discriminate between the two uses, dispatch first on the parent
context (typically the enclosing tag tells you whether you are inside a type,
an expression, or a substructure) and only then call the appropriate accessor.

### Building with the enum classes

`withTree` accepts any of the five enum types directly:

```nim
result.withTree StmtsS, info:                 # NimonyStmt
  result.withTree CallX, info:                # NimonyExpr
    result.addIdent "echo"
    result.addStrLit "hello"
```

Passing the enum rather than a raw string means a typo becomes a compile error
in the plugin rather than a malformed NIF tree at runtime.


## How plugins are compiled and run

1. The parser (nifler) lists every `.plugin` pragma in the module's deps file,
   and the dependency scanner resolves the path relative to the source file
   containing the pragma string literal.
2. The plugin is compiled with Nimony as a node of the build graph: nifmake
   builds the executable exactly once, before any of the module compilations
   that may run it (the declaring module and everything importing it), and
   rebuilds it — and them — when the plugin's source changes. The executable
   is cached in the nimcache directory. Should a plugin reach the compiler
   without a build-graph node (a pragma the parser could not see), it is
   built on demand instead; that path shares nothing between processes, so
   concurrent compilations of one module set stay safe.
3. At the call site, the compiler writes the input AST to a `.nif` file and invokes
   the plugin executable as a subprocess.
4. The plugin reads the input, transforms it, writes output, and exits.
5. The compiler reads the output NIF and splices it back into the AST.

Plugins are deterministic: same input produces same output. The compiler caches
results and skips re-execution when possible — the cache key is the input tree,
so a plugin that reads a file its input does not mention has to say so with
`dependsOn` (see [Reading files](#reading-files)).

The input files are written in NIF's *binary* form (bif) for speed — the names
keep their `.nif` extension and the plugin API sniffs the file header, so it
transparently accepts text NIF too (hand-written inputs keep working). To
inspect a cached input, decode it with `tools/niftools bif2nif <file>`.


## Plugin search

The path in `.plugin: "path"` is relative to the directory of the source file
that contains the string literal — not the call site. This matters when a
plugin pragma lives inside an imported template:

`/dirA/a.nim`:
```nim
template foo =
  {.plugin: "myplugin".}
```

`/dirB/b.nim`:
```nim
import ../dirA/a
foo()
```

The compiler searches for `myplugin.nim` in `/dirA/` (and in the standard library paths).


## Template plugins

A template plugin is invoked each time the template is called. The plugin receives
only the code related to the template invocation — typically the arguments.

```nim
template generateEcho(s: string) {.plugin: "deps/mplugin1".}

generateEcho("Hello, world!")
```

The input is wrapped in a `StmtsS` node containing the template name and
arguments. Use `callArgs` to obtain a bounded cursor over the arguments:

```nim
let input = loadPluginInput()
var args = callArgs(input)
# args now points to the first argument
```

A template plugin can also accept a code block via `typed` parameters:

```nim
template renderTree(s: typed) {.plugin: "deps/mrender".}

renderTree:
  let x = 1
  echo x
```

Here the plugin receives the entire block as typed, semantically checked NIF.


### Inspecting types: `needTypes`

Type arguments reach a plugin already sem-checked, but only *structural* types
arrive expanded:

| call | what the plugin sees |
|------|----------------------|
| `f(array[3, int])` | `(array (i 64) (rangetype (i 64) 0 2))` |
| `f(ptr int)` | `(ptr (i 64) (notnil))` |
| `f(tuple[a: int])` | `(tuple (kv a (i 64)))` |
| `f(Foo[int])` | `Foo.0.Iw3zqsv.mymod` |
| `f(MyDistinct)` | `MyDistinct.0.mymod` |

A nominal type — object, enum, distinct, generic instance — is a bare `Symbol`,
and a plugin cannot resolve it: it runs in its own process, and the module it
would have to read is still being sem-checked.

So it asks. Returning `needTypes(syms)` as the plugin's whole output makes the
compiler append those declarations to the second input and run the plugin again:

```nim
var defs = loadTypeDefinitions()   # (stmts (type :Sym … )*), empty at first
...
if missing.len > 0:
  return needTypes(missing)
```

Nothing is shipped that was not asked for, so a plugin that never inspects types
pays nothing. Batch each level into one request — a round costs a re-run, so a
plugin walking a type graph should ask for a whole level at a time rather than
one symbol at a time.

Two properties make the exchange safe:

- Asking again for a declaration already provided is a compile-time error. Every
  round must name something new, so the loop is finite by construction.
- A symbol that turns out to be a *type variable* is answered with its
  `(typevar …)` declaration rather than silence. A plugin can therefore
  recognize `T` positively instead of inferring it from absence — which would
  misread every not-yet-requested type as a type variable.

A generic instance's `<typevars>` slot holds `(at <head> <args>...)`, so
`genericHead` and `genericParams` fall straight out of it.

`lib/std/typetraits.nim` is implemented this way, with no compiler magic at all;
`lib/std/deps/typetraits.nim` is the plugin.


### Types in, types out

A template plugin declared to return `typedesc` may be used in type position,
and its output is re-checked as a type:

```nim
type Q = distinctBase(MyOtherInt)
var y: genericHead(Foo[int])[float]
```

Inside a generic body the plugin runs **once**, on the generic body, with the
*typevar* as its argument — not once per instantiation. Its output then takes
part in ordinary substitution, so a plugin that transforms types symbolically
(`pointerBase(ptr T)` → `T`) is correct there, and cheaper: one expansion
instead of one per instantiation.

A plugin that has to *decide* something from the concrete type cannot answer
that way. It says so, by returning `deferExpansion()` as its whole output:

```nim
if mentionsTypevar(defs, arg):
  return deferExpansion()
```

The compiler then leaves the call in the tree as `(at <template> <args>…)` —
nimony's ordinary unresolved type application, the same shape `Foo[T]` takes in
a generic body. Instantiation substitutes into it and re-checks it, which asks
the plugin again with concrete types:

```nim
proc unwrap[T](x: T): distinctBase(T) =
  result = distinctBase(T)(x)

echo unwrap(MyInt(12))      # plugin asked again with MyInt  -> int
echo unwrap(MyFloat(2.5))   # ... and with MyFloat           -> float
```

Deferring is rejected — a hard error — when no argument contains a generic
parameter, since nothing would be substituted before the next round and the
plugin would be asked the same question forever.

Until it is resolved, a deferred type is exactly as usable as a bare type
variable: it can be declared, passed, converted and returned, but operations
that need to look through it (`echo` on a value of that type, field access)
fail in the generic body for the same reason they fail for `T` itself.


## Module plugins

A module plugin receives the **entire module** after semantic analysis. It must
output back the complete module, with transformations applied:

```nim
{.plugin: "deps/mmoduleplugin".}

echo "this should not be erased"

block:
  echo "should be erased"
```

The plugin can selectively strip, rewrite, or augment any part of the module.
For example, stripping all top-level `block` statements:

```nim
import plugins

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  let info = n.info
  var body = firstChild(n)
  result.withTree StmtsS, info:
    while body.hasMore:
      if body.kind == TagLit and body.stmtKind == BlockS:
        skip body  # remove it
      else:
        result.takeTree body

var inp = loadPluginInput()
saveTree transform(inp)
```

Plugins can be hidden inside imported modules so that callers don't see the
`.plugin` pragma:

```nim
# deps/mhiddenplugin.nim
template eraseToplevelBlocks* =
  {.plugin: "mmoduleplugin".}
```

```nim
# user.nim
import deps/mhiddenplugin
eraseToplevelBlocks()  # expands to {.plugin: "mmoduleplugin".} and thus invokes the plugin
```


## Type plugins

A type plugin is attached to a nominal type and is invoked for **every module**
that uses the type. This enables type-driven code transformations similar to
Nim 2's term rewriting macros.

The type plugin receives two inputs:
- `loadPluginInput()`: the module AST
- `loadTypeDefinitions()`: the type definition(s) that triggered the plugin

```nim
# listener.nim
type
  Listener* {.plugin: "mtypeplugin".} = object
    i*: int
    s*: string
```

```nim
# user.nim
import listener

template onChanged(x: var Listener, field: untyped, name: string): untyped =
  echo name, " changed to ", field

var p = Listener(i: 1, s: "a")
p.i = 2   # prints: i changed to 2
p.s = "b" # prints: s changed to b
```

The plugin intercepts assignments to fields of `Listener` and injects calls to the
`onChanged` template.

Another use case is expression template optimization. A type like `Matrix` with a
plugin can rewrite `a*b + c - d` into fused in-place operations, avoiding temporaries:

```nim
type
  Matrix {.plugin: "avoidtemps".} = object
    a: array[4, array[4, float]]
```


## Import plugins

The `import` statement can be combined with a plugin pragma to load a module
whose NIF is produced by a plugin:

```nim
import (path/foo) {.plugin: "v2".}
```

This is used for compatibility layers that translate between different NIF formats.

Import plugins do not follow the rules above: the string is **not** a path to a
`.nim` file resolved against the importing module, and the compiler does not
build it. It names an already-built executable, looked up first relative to the
current directory and then in the toolchain's `bin` directory. The plugin is run
as `<plugin> <path/foo.nim> <nimcache/<plugin>/<module>.s.nif>` and is
responsible for filling `nimcache/<plugin>` with the semchecked NIF **and** the
matching index files.

The `v2` plugin (`src/v2`) is the in-tree example; it shells out to a Nim 2
compiler to translate Nim 2 modules. Build it with `nim c src/v2/v2.nim`.


## The plugin API (`plugins`)

Plugins import a single module:

```nim
import plugins
```

### Reading input

```nim
proc loadPluginInput*(filename = paramStr(1)): NifCursor
```

Returns a `NifCursor` positioned at the root of the NIF tree. Type plugins use
`loadPluginInput()` for the module and `loadTypeDefinitions()` for the
definitions that triggered the plugin.


### NifCursor — reading NIF trees

A `NifCursor` is a reference-counted, read-only cursor into a frozen NIF tree.
It advances forward only. Child cursors are bounded by `nifcore` jump counts;
there is no closing-token value.

All inputs and builders in one plugin process share a pre-seeded literal and tag
pool. Consequently, `SymId` values are directly comparable across the main input,
type-definition input, and generated builders, and Nimony enum ordinals map
directly to `TagId`.

`SymId` and `TagId` are pool-local numeric handles. Use `symText` and `tagText`
when textual names are needed; `$id` is intentionally not a text lookup.

**Navigation:**

| Proc | Description |
|------|-------------|
| `inc(n)` | Advance by one token |
| `skip(n)` | Skip current token or entire subtree |

**Inspecting the current token:**

| Proc | Returns | Description |
|------|---------|-------------|
| `kind(n)` | `NifKind` | Raw token kind (`TagLit`, `Symbol`, `Ident`, `StrLit`, `IntLit`, ...) |
| `info(n)` | `LineInfo` | Source location |
| `stmtKind(n)` | `NimonyStmt` | Statement kind (`VarS`, `ProcS`, `CallS`, ...) or `NoStmt` |
| `exprKind(n)` | `NimonyExpr` | Expression kind (`CallX`, `DotX`, `AddX`, ...) or `NoExpr` |
| `typeKind(n)` | `NimonyType` | Type kind (`ObjectT`, `ArrayT`, ...) or `NoType` |
| `otherKind(n)` | `NimonyOther` | Substructure kind or `NoSub` |
| `pragmaKind(n)` | `NimonyPragma` | Pragma kind or `NoPragma` |
| `tagId(n)` | `TagId` | Raw NIF tag id for `TagLit` tokens |
| `tagText(n)` | `string` | Textual tag name |
| `symId(n)` | `SymId` | Opaque symbol handle |
| `symText(n)` | `string` | Symbol text |
| `identText(n)` | `string` | Identifier text |
| `stringValue(n)` | `string` | String literal value |
| `intValue(n)` | `BiggestInt` | Integer literal value |
| `uintValue(n)` | `BiggestUInt` | Unsigned integer value |
| `floatValue(n)` | `BiggestFloat` | Float literal value |
| `charLit(n)` | `char` | Character literal |
| `eqIdent(n, name)` | `bool` | True if token matches `name` (works for idents and symbols) |

**Source location helpers:**

| Proc | Description |
|------|-------------|
| `isValid(info)` | True when info refers to a real source location |
| `filePath(info)` | Source file path, or `""` |
| `lineCol(info)` | `SourcePos(line, col)` (1-based), or `(0, 0)` |


### NifBuilder — building NIF output

A `NifBuilder` is a move-only alias for `nifcore.TokenBuf`. It cannot be copied.
`snapshot(tree)` returns a reference-counted cursor; mutating the builder after a
snapshot transparently detaches its token storage.

**Creating trees:**

| Proc | Description |
|------|-------------|
| `createTree()` | Empty mutable tree |
| `snapshot(tree)` | Read-only `NifCursor` into the tree (keeps tree alive) |
| `isEmpty(tree)` | True when tree has no tokens |

**Appending tokens:**

| Proc | Description |
|------|-------------|
| `openTree(t, tag, info)` | Begin a tagged tree with optional line info |
| `openTree(t, tagString, info)` | Same, from a textual tag name |
| `closeTree(t)` | Close the most recently opened tree |
| `addIdent(t, name)` | Identifier |
| `addSymDef(t, sym, info)` | Symbol definition (from `SymId` or string) |
| `addSymUse(t, sym, info)` | Symbol reference (from `SymId` or string) |
| `genSym()` | Fresh local `SymId` for `addSymDef` and `addSymUse` |
| `addStrLit(t, s)` | String literal |
| `addIntLit(t, i)` | Signed integer literal |
| `addUIntLit(t, u)` | Unsigned integer literal |
| `addFloatLit(t, f)` | Float literal |
| `addCharLit(t, c)` | Character literal |
| `addDotToken(t)` | Dot placeholder |
| `addEmptyNode(t)` | Single empty placeholder |

**Copying from input:**

| Proc | Description |
|------|-------------|
| `takeTree(t, n)` | Copy the current subtree from `n` into `t`, advancing `n` |
| `addSubtree(t, n)` | Copy subtree from `n` into `t` without advancing |
| `addTree(t, child)` | Append an entire child builder without consuming it |

`genSym()` uses the input file's standard NIF `.unusedname` hint. The returned
`SymId` can be emitted through the ordinary symbol operations:

```nim
let temp = genSym()
result.withTree LetS, info:
  result.addSymDef temp, info
  result.addEmptyNode3(info)
  result.takeTree value
result.withTree CallS, info:
  result.addIdent "echo"
  result.addSymUse temp, info
```

**Structured building:**

```nim
result.withTree StmtsS, info:
  result.withTree CallS, info:
    result.addIdent "echo"
    result.addStrLit "hello"
```

`withTree` opens a node of the given kind, runs the body, and closes it.


### Writing output

```nim
proc saveTree*(tree: sink NifBuilder)                  # writes to paramStr(2)
proc saveTree*(tree: sink NifBuilder; filename: string)
proc renderTree*(tree: var NifBuilder): string         # debug rendering (no line info)
```


### Reading files

```nim
proc dependsOn*(path: string)
```

A plugin's output is cached under a checksum of its *input tree*, so a plugin
that reads anything else — a schema, a template, a table of constants — would
otherwise be memoized forever: the file changes, the input does not, and the
build keeps the old expansion. `dependsOn` reports the read, and the compiler
re-runs the plugin (and re-checks the module) when the file changes.

```nim
proc tr(n: NifCursor): NifBuilder =
  let schema = getCurrentDir() / "schema.json"
  dependsOn schema
  ...
```

Call it for every file the plugin opens, before or after the read; reporting a
path that does not exist is harmless. Paths are made absolute against the
plugin's working directory, which is the compiler's.

Two limits are worth knowing:

* Only files that exist when the compiler reads the report are tracked, so
  *creating* a file the plugin looked for and did not find invalidates nothing.
  Deleting a tracked one does force a rebuild.
* Naming a **directory** tracks add/remove of its direct entries and nothing
  else, because that is all a directory mtime says. To follow the contents of a
  tree, name each file.

Under the hood the paths travel back as a leading `(dependency …)` sidecar tree
next to the gensym hint, are peeled off before the output becomes part of the
module, and land in the module's `.s.deps.nif` as extra inputs of the next
build's `nimsem` node.


### Error reporting

```nim
proc errorTree*(msg: string): NifBuilder
proc errorTree*(msg: string; info: LineInfo): NifBuilder
proc errorTree*(msg: string; at: NifCursor): NifBuilder
proc errorTree*(msg: string; at, orig: NifCursor): NifBuilder
```

Return an `ErrT` node that the compiler reports as a compile-time error. The
`info` overload provides source location only; `at` provides both source location
and embedded source context; `orig` lets the diagnostic location and embedded
source context differ.


### Validation

The compiler performs a validation of the plugin code. If the plugin code is invalid, it is rejected. The details of this validation process are still work-in-progress and deliberately not documented yet.

## Typical plugin structure

### Using the Replacer API (recommended)

The `Replacer` type bundles an input cursor and output builder into a single
object with balanced operations. Each operation consumes input and produces
output atomically, making cursor/builder mismatches impossible:

```nim
import plugins

proc trAux(r: var Replacer) =
  if r.isAtom:
    keep r, Any             # pass through leaf tokens
  else:
    case r.stmtKind
    of SomeStmtToTransform:
      # ... custom transformation ...
    else:
      loopKeepTag r:       # descend and recurse
        trAux r

var r = loadReplacer()
loopKeepTag r:             # enter top-level (stmts ...) and recurse
  trAux r
saveReplacer(r)
```

The core operations are:
- `keep r, Kind` — copy one child verbatim, asserting its kind
- `drop r, Kind` — skip one child without emitting, asserting its kind
- `replace r, Kind, node` — skip one child of `Kind`, emit a replacement (`NifCursor` or borrowed `NifBuilder`)
- `keepTag r:` — descend into a compound node (copy tag, process children, close)
- `loopKeepTag r:` — copy tag, iterate all children, close
- `peek r:` — read-ahead analysis without consuming (cursor is restored)
- `getCursor(r)` / `setCursor(r, c)` — snapshot/restore cursor for analysis

Kind annotations are mandatory: `keep r, Expr`, `drop r, Type`, `keep r, AsgnS`.
Use `Any` when the child can be anything, or specific tags like `AsgnS`, `MutT`
when the grammar dictates a fixed child.

Use `r.dest` for direct builder access when emitting new nodes:

```nim
r.dest.withTree CallS, info:
  r.dest.addSymUse sym, info
  r.dest.addStrLit "hello"
```

### Low-level API

The `NifCursor` and `NifBuilder` types remain available for plugins that
construct output from scratch rather than transforming input:

```nim
import plugins

proc transform(n: NifCursor): NifBuilder =
  result = createTree()
  var n = n
  if n.stmtKind == StmtsS:
    n = firstChild(n)
  result.withTree StmtsS, n.info:
    while n.hasMore:
      result.takeTree n

var inp = loadPluginInput()
saveTree transform(inp)
```

The key low-level operations are:
- `takeTree` to pass nodes through unchanged
- `skip` to remove nodes
- `withTree` or `openTree`/`closeTree` to construct new nodes
- `addSubtree` to duplicate nodes

### Traversal and transformation templates

Templates for tree traversal (read-only) and transformation (read+write).
The `into`/`loopInto` templates are for pure analysis; the
`keepTag`/`loopKeepTag` templates copy the node tag to the output.

| Template | Type | Purpose |
|---|---|---|
| `hasMore(n)` | `NifCursor` | true while the bounded cursor has more values |
| `into n:` | `NifCursor` | enter a `TagLit`, run the body for its children, leave |
| `loopInto n:` | `NifCursor` | enter node, iterate all children, leave |
| `linearScan n:` | `NifCursor` | scan descendant compound nodes, then advance past the subtree |
| `keepTag r:` | `Replacer` | copy tag to output, run body, close tree and advance |
| `loopKeepTag r:` | `Replacer` | copy tag, iterate all children, close tree and advance |
