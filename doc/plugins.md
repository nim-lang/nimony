# Plugins

Plugins are Nimony's metaprogramming mechanism, replacing Nim 2's macro system.
A plugin is a separate Nim program that transforms NIF (Nimony Intermediate Format)
trees at compile time. Plugins run as external processes, communicating with the
compiler through NIF files.

## Overview

There are three kinds of plugins:

| Kind | Declaration | Scope |
|------|-------------|-------|
| **Template plugin** | `template foo(...) {.plugin: "path".}` | Invoked at each call site |
| **Module plugin** | `{.plugin: "path".}` as statement | Transforms the entire module |
| **Type plugin** | `type T {.plugin: "path".} = ...` | Invoked for every module that uses `T` |

All plugins share the same API (`nimonyplugins`) and execution model.


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
import nimonyplugins

proc transform(n: Node): NifBuilder =
  result = createTree()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.withTree StmtsS, info:
    result.withTree CallS, info:
      result.addIdent "echo"
      result.takeTree n

var inp = loadPluginInput()
saveTree transform(inp)
```

The plugin reads the template arguments as a NIF tree from `paramStr(1)`, transforms
them, and writes the result to `paramStr(2)`.


## How plugins are compiled and run

1. The compiler finds the `.plugin` pragma and resolves the path relative to the
   source file containing the pragma string literal.
2. The plugin is compiled with **Nim 2** (not Nimony): `nim c -d:nimonyPlugin plugin.nim`.
   The compiled executable is cached in the nimcache directory and reused until the source changes.
3. At the call site, the compiler writes the input AST to a `.nif` file and invokes
   the plugin executable as a subprocess.
4. The plugin reads the input, transforms it, writes output, and exits.
5. The compiler reads the output NIF and splices it back into the AST.

Plugins are deterministic: same input produces same output. The compiler caches
results and skips re-execution when possible.

**Note:** Plugins are compiled with Nim 2 because Nimony is not yet considered
stable enough to compile itself.


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

The input `Node` is wrapped in a `StmtsS` node containing the arguments. A typical
plugin starts by skipping past this wrapper:

```nim
var n = loadPluginInput()
if n.stmtKind == StmtsS: inc n
# n now points to the first argument
```

A template plugin can also accept a code block via `typed` parameters:

```nim
template renderTree(s: typed) {.plugin: "deps/mrender".}

renderTree:
  let x = 1
  echo x
```

Here the plugin receives the entire block as typed, semantically checked NIF.

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
eraseToplevelBlocks()  # invokes the plugin
```


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
import nimonyplugins

proc transform(n: Node): NifBuilder =
  result = createTree()
  let info = n.info
  var n = n
  if n.stmtKind == StmtsS: inc n
  result.withTree StmtsS, info:
    while n.kind != ParRi:
      if n.kind == ParLe and n.stmtKind == BlockS:
        skip n  # remove it
      else:
        result.takeTree n

var inp = loadPluginInput()
saveTree transform(inp)
```


## Type plugins

A type plugin is attached to a nominal type and is invoked for **every module**
that uses the type. This enables type-driven code transformations similar to
Nim 2's term rewriting macros.

The type plugin receives two inputs:
- `paramStr(1)`: the module AST
- `paramStr(3)`: the type definition(s) that triggered the plugin

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
import (path/foo) {.plugin: "std/v2".}
```

This is used for compatibility layers that translate between different NIF formats.


## The plugin API (`nimonyplugins`)

Plugins import a single module:

```nim
import nimonyplugins
```

### Reading input

```nim
proc loadPluginInput*(filename = paramStr(1)): Node
```

Returns a `Node` positioned at the root of the NIF tree. For type plugins, load
the type definitions with `loadPluginInput(paramStr(3))`.


### Node — reading NIF trees

A `Node` is a read-only cursor into a frozen NIF tree. It advances forward only.

**Navigation:**

| Proc | Description |
|------|-------------|
| `inc(n)` | Advance by one token |
| `skip(n)` | Skip current token or entire subtree |

**Inspecting the current token:**

| Proc | Returns | Description |
|------|---------|-------------|
| `kind(n)` | `NifKind` | Raw token kind (`ParLe`, `ParRi`, `Symbol`, `Ident`, `IntLit`, ...) |
| `info(n)` | `LineInfo` | Source location |
| `stmtKind(n)` | `NimonyStmt` | Statement kind (`VarS`, `ProcS`, `CallS`, ...) or `NoStmt` |
| `exprKind(n)` | `NimonyExpr` | Expression kind (`CallX`, `DotX`, `AddX`, ...) or `NoExpr` |
| `typeKind(n)` | `NimonyType` | Type kind (`ObjectT`, `ArrayT`, ...) or `NoType` |
| `otherKind(n)` | `NimonyOther` | Substructure kind or `NoSub` |
| `pragmaKind(n)` | `NimonyPragma` | Pragma kind or `NoPragma` |
| `tagId(n)` | `TagId` | Raw NIF tag id for `ParLe` tokens |
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

A `NifBuilder` is a mutable builder. Copy-on-write semantics: copying a tree shares
the buffer until the next mutation detaches it.

**Creating trees:**

| Proc | Description |
|------|-------------|
| `createTree()` | Empty mutable tree |
| `createTree(kind, children...)` | Tree node of `kind` containing `children` |
| `createTree(kind, info, children...)` | Same, with line info |
| `snapshot(tree)` | Read-only `Node` into the tree (keeps tree alive) |
| `isEmpty(tree)` | True when tree has no tokens |

**Appending tokens:**

| Proc | Description |
|------|-------------|
| `addParLe(t, tag, info)` | Opening `(tag` with optional line info |
| `addParLe(t, tagString, info)` | Same, from textual tag name |
| `addParRi(t)` | Closing `)` |
| `addIdent(t, name)` | Identifier |
| `addSymUse(t, sym, info)` | Symbol reference (from `SymId` or string) |
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
| `takeTree(t, n)` | Move current subtree from `n` into `t`, advancing `n` |
| `addSubtree(t, n)` | Copy subtree from `n` into `t` without advancing |
| `add(t, child)` | Append entire `child` tree to `t` |

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
proc saveTree*(tree: NifBuilder)                  # writes to paramStr(2)
proc saveTree*(tree: NifBuilder; filename: string)
proc renderTree*(tree: NifBuilder): string        # debug rendering (no line info)
```


### NIF templates and interpolation

For quick tree construction, use NIF string templates with `%~` interpolation:

```nim
let greeting = """(call echo $msg)""" %~ {"msg": ~"hello"}
```

The `$name` placeholders are replaced with tree fragments from the bindings. The `~`
operator converts Nim values to tree fragments:

| Expression | Produces |
|-----------|----------|
| `~node` | Copy of `Node` subtree |
| `~tree` | The `NifBuilder` unchanged |
| `~"hello"` | String literal |
| `~ident("foo")` | Identifier (not a string literal) |
| `~42` | Integer literal |
| `~3.14` | Float literal |
| `~'x'` | Character literal |
| `~true` | Boolean keyword node |

Use `$$` for a literal dollar sign.

Templates compose naturally:

```nim
let first = """(call echo $arg)""" %~ {"arg": ~n}
let second = """(call echo $msg)""" %~ {"msg": ~"done"}

result = """(stmts $a $b)""" %~ {"a": ~first, "b": ~second}
```

For templates without substitutions, use `nifFragment`:

```nim
let tail = nifFragment("""(call echo "done")""")
```


### Error reporting

```nim
proc errorTree*(msg: string): NifBuilder
proc errorTree*(msg: string; at: Node): NifBuilder
proc errorTree*(msg: string; at, orig: Node): NifBuilder
```

Return an `ErrT` node that the compiler reports as a compile-time error. The `at`
parameter provides source location; `orig` embeds the original source for context.


### Validation

The API automatically validates trees constructed via `createTree(kind, children...)`,
`%~`, and `nifFragment`. If a constructed tree has wrong structure (e.g. missing
children for a `CallS` node), it is replaced with an `ErrT` node describing the problem.
Trees built manually via `addParLe`/`addParRi` are not validated.


## Typical plugin structure

Most plugins follow this pattern:

```nim
import nimonyplugins

proc transform(n: Node): NifBuilder =
  result = createTree()
  var n = n
  # Skip the StmtsS wrapper
  if n.stmtKind == StmtsS: inc n
  result.withTree StmtsS, n.info:
    while n.kind != ParRi:
      case n.kind
      of ParLe:
        case n.stmtKind
        of SomeStmtToTransform:
          # ... custom transformation ...
        else:
          result.takeTree n  # pass through unchanged
      else:
        result.takeTree n

var inp = loadPluginInput()
saveTree transform(inp)
```

The key operations are:
- `takeTree` to pass nodes through unchanged
- `skip` to remove nodes
- `withTree`/`addParLe`/`addParRi` to construct new nodes
- `addSubtree` to duplicate nodes
