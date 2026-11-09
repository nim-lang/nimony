# Small metaprogramming recipes

These examples introduce the NIF trees that a template plugin receives and
returns. Start by reading a tree, then copy an expression, then inspect a block
and report an error. The runnable examples are in
[`tests/nimony/plugins`](../tests/nimony/plugins).

## 1. Read a NIF tree

NIF represents syntax as nested nodes. A node begins with its tag, followed by
its children inside parentheses. For example, the shape of a function call is:

```text
(call FUNCTION ARGUMENT...)
```

`FUNCTION` and `ARGUMENT` are explanatory placeholders here. For example:

```text
(call twice.0.demo 3)
```

Here `call` is the tag, `twice.0.demo` is a symbol token naming a procedure,
and `3` is its integer argument. The actual symbol suffix depends on the
compiler's name assignment and the module. A call has one
callee and zero or more arguments. A statement list has the shape
`(stmts STATEMENT...)`. An integer literal such as `42` is an atom, with no
children.

A template plugin receives a wrapper with this shape:

```text
(stmts TEMPLATE_NAME ARGUMENT...)
```

For the `unchanged(nextValue(calls))` example below, its argument is a call
node. Because the template parameter is `typed`, the compiler has already
checked the expression and resolved its names. The callee and variable are
symbol tokens identifying declarations; they are not just the source spellings
`nextValue` and `calls`. Compiler-added nodes may also appear in typed input.

Use `renderNode(n)` to inspect the textual NIF representation of a cursor.
See [the tag reference](tags.md) for node layouts, and the
[plugin guide](plugins.md) for cursor and builder operations.

## 2. Return one expression unchanged

Create `deps/mcookbook_identity.nim`:

```nim
import plugins

proc transform(input: NifCursor): NifBuilder =
  var expression = callArgs(input)
  result = createTree()
  result.takeTree expression

saveTree transform(loadPluginInput())
```

A plugin is a separate program. Nimony builds and runs it during compilation.
`loadPluginInput()` loads the compiler's input tree. `callArgs(input)` skips
the wrapper and template name and returns a cursor at the first argument.
This template has exactly one argument.

`createTree()` creates the output builder. `takeTree` copies the complete
subtree at the cursor into it and advances that cursor past the subtree.
That is why `expression` is declared with `var`. Copying the existing subtree
preserves its resolved symbols and source positions. `saveTree` writes the
output for the compiler to use as the template expansion.

Create a client next to the `deps` directory:

```nim
import std / syncio

template unchanged(value: typed): untyped {.plugin: "deps/mcookbook_identity".}

proc nextValue(calls: var int): int =
  inc calls
  calls

var calls = 0
echo unchanged(nextValue(calls))
echo "calls: ", calls
```

Output:

```text
1
calls: 1
```

The plugin runs during compilation and copies the call's syntax once.
`nextValue` runs when the compiled program executes; it increments `calls`
once. The plugin also accepts atoms such as `unchanged(42)` because `takeTree`
handles both atoms and compound nodes.

## 3. Inspect calls and report an error

Now walk a typed block and reject direct calls whose resolved symbol has the
basename `blocked`. Create `deps/mcookbook_check.nim`:

```nim
import plugins

proc rejected(n: NifCursor): NifBuilder =
  ## A small syntactic rule: reject a direct call whose resolved name is blocked
  if n.exprKind == CallX:
    let call = asCall(n)
    if call.fn.kind == Symbol and symBasename(call.fn.symId) == "blocked":
      return errorTree("checked block may not call blocked", n)
  for child in n.children:
    let err = rejected(child)
    if not err.isEmpty: return err
  result = createTree()

proc transform(input: NifCursor): NifBuilder =
  var body = callArgs(input)
  let err = rejected(body)
  if not err.isEmpty: return err
  result = createTree()
  result.takeTree body

saveTree transform(loadPluginInput())
```

`exprKind` identifies a node's expression tag. After checking for `CallX`,
`asCall(n).fn` gives the callee cursor. The callee may itself be an expression;
this example tests for a `Symbol` before reading its name.

`children` visits each immediate child. Calling `rejected` recursively on those
children also finds calls inside nested statements and expressions. Each
successful visit returns an empty builder. An error visit returns
`errorTree(message, n)`, which attaches the diagnostic to the offending node.
The first nonempty error is returned to the compiler.

When the block passes, `transform` copies the original block into the output,
just as the previous plugin copied one expression.

Client:

```nim
template checked(body: typed) {.plugin: "deps/mcookbook_check".}

proc blocked() = discard

checked:
  blocked()
```

The compiler reports `checked block may not call blocked` at `blocked()`.
A block containing `echo "checked"` instead is accepted and executes normally.

This rule compares basenames, so any direct callee named `blocked` is rejected,
even if it comes from another module. It does not inspect the bodies of called
procedures or resolve indirect calls. It is a small tree-inspection example.

## 4. Bind names in an ordinary template

`bind` resolves a name in the template's definition context. In Nimony,
procedures do not need forward declarations: a procedure written later in the
same module can already be in scope when `bind` is processed.

Consider this complete program:

```nim
import std / syncio

template appendBang(target: var string) =
  bind add
  add(target, "!")

proc add(target: var string; value: string) =
  target = "intercepted"

var text = "base"
appendBang(text)
echo text
```

Nimony prints:

```text
intercepted
```

The local `add` procedure is already in scope when `bind add` is processed,
even though its declaration appears below the template. It is the procedure
selected in this example. Moving down the source file does not put that
procedure outside the template's definition context.

Nim 2.2.12 prints `base!` for this program. This difference was reported in
[issue #2877](https://github.com/nim-lang/nimony/issues/2877). The maintainer
[explained](https://github.com/nim-lang/nimony/issues/2877#issuecomment-6623571306)
that the Nimony result is intentional because the later procedure is already
in scope.

To see a separate calling context, put this code in
`deps/cookbook_binding.nim`:

```nim
proc label(value: string): string =
  "library: " & value

template boundLabel*(value: string): string =
  bind label
  label(value)
```

Then use the exported template from a client module:

```nim
import std / syncio
import deps / cookbook_binding

proc label(value: string): string =
  "caller: " & value

echo label("example")
echo boundLabel("example")
```

Output:

```text
caller: example
library: example
```

The direct call uses the client's `label`. Inside `boundLabel`, `bind label`
resolves to the helper module's private procedure. The client's same-spelled
procedure does not replace that binding. Ordinary template hygiene also
influences name lookup; this example focuses on explicit binding.

These are ordinary templates and need no plugin. The plugin builder operation
`bindSym` is a separate API; these examples concern the template statement
`bind`.

## Run the fixtures

From the repository root, with a toolchain matching the plugin API:

```sh
bin/nimony c -r tests/nimony/plugins/tcookbook.nim
bin/nimony c tests/nimony/plugins/tcookbook_bad.nim
bin/nimony c -r tests/nimony/plugins/tcookbook_bind_local.nim
bin/nimony c -r tests/nimony/plugins/tcookbook_bind_import.nim
```

The second command is expected to fail at its `blocked()` call. The `.output`
and `.msgs` sidecars record the expected results for the compiler's test runner.
The positive fixture also checks a literal argument and nested uses of
`unchanged`, including the runtime evaluation count.
