# Small metaprogramming recipes

Start with a template when the generated code has a fixed shape. Use a plugin
when it needs to inspect syntax or declarations. A plugin can emit a call to
a template instead of constructing the template's body node by node.

These examples use the official `plugins` API. The runnable fixtures live in
[`tests/nimony/plugins`](../tests/nimony/plugins).

## 1. Debug an expression with an ordinary template

The `dbg` template in `deps/cookbook_support.nim` prints an expression and its
value. `astToStr(value)` inspects its syntax without evaluating it; `value`
appears once in the evaluated part of the expansion.

```nim
import std / syncio

template dbg(value: untyped) =
  bind astToStr, echo
  echo astToStr(value), " = ", value

proc nextValue(calls: var int): int =
  inc calls
  calls

var calls = 0
dbg(nextValue(calls))
echo "calls: ", calls
```

Output:

```text
nextValue(calls) = 1
calls: 1
```

No plugin or parser dependency is needed for this fixed expansion. The printed
expression is the compiler's representation, not a byte-for-byte copy of the
source. `bind` names the operations used by the template; it is not a promise
of identical overload visibility to Nim 2, which has different declaration
ordering rules. When a plugin must fix a particular set of overloads, pass
explicitly bound AST arguments as in the next example.

## 2. Inspect object fields, then call helper templates

A plugin can request an object's declaration, inspect its fields, and emit a
call to a template that defines `describe`. The template contains ordinary Nim
code; only the variable-shaped list of fields is assembled by the plugin.

The helper file `deps/cookbook_support.nim` contains:

```nim
template appendField*(target, label, value, append, concat, format: untyped) {.untyped.} =
  append(target, concat(label, format(value)))

template describeImpl*(publicName, parameter, T, heading, returnType, append, fields: untyped) {.untyped.} =
  proc publicName(parameter: T): returnType =
    result = heading
    fields
    append(result, ")")
```

The plugin `deps/mcookbook_describe.nim`:

```nim
import plugins
import cookbook_support

proc transform(input: NifCursor): NifBuilder =
  let typ = callArgs(input)
  if typ.kind != Symbol:
    return errorTree("describe needs a named object type", typ)
  let decl = findTypeDecl(loadTypeDefinitions(), typ.symId)
  if not decl.hasMore:
    return needTypes(typ.symId)
  let body = asTypeDecl(decl).body
  if body.typeKind != ObjectT:
    return errorTree("describe needs an object type", typ)
  let obj = asObjectDecl(body)
  if obj.parentType.kind != DotToken:
    return errorTree("this example supports objects without inheritance", typ)
  # Reject variant sections rather than silently leaving their fields out
  var first = true
  for child in body.children:
    if first: first = false
    elif child.otherKind != FldU:
      return errorTree("this example supports plain object fields only", child)
  result = createTree()
  result.withTree CallX, input.info:
    result.bindSym "describeImpl"
    result.addIdent "describe"
    result.addIdent "value"
    var typeNode = typ
    result.takeTree typeNode
    result.addStrLit symBasename(typ.symId) & "("
    result.bindSym "string"
    result.bindSym "add"
    result.withTree StmtsS, input.info:
      var firstField = true
      for field in obj.fields:
        result.withTree CallX, field.name.info:
          result.bindSym "appendField"
          result.addIdent "result"
          result.addStrLit (if firstField: "" else: ", ") & symBasename(field.name.symId) & ": "
          result.withTree DotX, field.name.info:
            result.addIdent "value"
            result.addIdent symBasename(field.name.symId)
          result.bindSym "add"
          result.bindSym "&"
          result.bindSym "$"
        firstField = false

saveTree transform(loadPluginInput())
```

The first invocation returns `needTypes`; the compiler supplies the declaration
and invokes the plugin again. `findTypeDecl` distinguishes that supplied type
from an absent declaration. This example demonstrates named objects with plain fields and no inheritance. `fields` only visits directly declared
plain fields, so the plugin rejects other layouts rather than omitting them.

The declaration name `describe` is deliberately resolved in the user's module.
Parameter, result and field names are deliberately coordinated with the helper
templates. In contrast, the helpers and string-formatting operations use
`bindSym`: the references resolve in the plugin's definition scope. Passing
`add`, `&` and `$` as AST arguments keeps formatting stable even if the user
has overloads with those spellings.

A client:

```nim
import std / syncio

template defineDescribe(T: typedesc) {.plugin: "deps/mcookbook_describe".}

type Person = object
  name: string
  age: int

defineDescribe(Person)
echo describe(Person(name: "Ann", age: 30))
```

It prints `Person(name: Ann, age: 30)`. See `tcookbook.nim` for an empty object
and fields named `result` and `value`, and `tcookbook_shadow.nim` for caller
formatting overloads.

## 3. Reject a call and preserve the rest of a typed block

A plugin can check a typed block and return it unchanged when it passes. This
example rejects direct calls whose resolved symbol has basename `blocked`.
It is a deliberately small syntactic check, not a sandbox: indirect calls,
callee bodies and symbol identity across modules are outside its rule.

`deps/mcookbook_check.nim`:

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

Client:

```nim
template checked(body: typed) {.plugin: "deps/mcookbook_check".}

proc blocked() = discard

checked:
  blocked()
```

`errorTree(message, node)` attaches the diagnostic to the rejected call. The
compiler reports `checked block may not call blocked` at `blocked()`.
To accept the block, return its original subtree with `takeTree`; rebuilding
names as strings would lose symbol identity and potentially private-access
metadata.

## Run the examples

From the repository root, use a toolchain with the matching plugin API:

```sh
bin/nimony c -r tests/nimony/plugins/tcookbook.nim
bin/nimony c -r tests/nimony/plugins/tcookbook_shadow.nim
bin/nimony c tests/nimony/plugins/tcookbook_bad.nim  # expected error
```

The `.output` and `.msgs` sidecars make these examples part of the compiler's
test suite. The negative fixture must fail at its offending call, not merely
fail to compile for an unrelated reason.

## Temporary names and preserved user code

The examples deliberately name public declarations such as `describe`. For a
private temporary introduced by a plugin, use `genSym`, `addSymDef` and
`addSymUse`, as in
[`mplugin_gensym.nim`](../tests/nimony/plugins/deps/mplugin_gensym.nim).
Creating an identifier spelled `tmp` does not make it fresh. Likewise, copy a
user-supplied expression with `takeTree` rather than rebuilding its identifiers;
its already-resolved symbols and source locations belong to that expression.
