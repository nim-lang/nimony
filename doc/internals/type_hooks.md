# How `=hooks` are attached to types

Shoggoth needs a type's lifetime hooks to inject dups, copies, moves and
destructors. It cannot look a hook up by name or scope, because it no longer
has sem's scopes. Instead, the **type declaration in the Shoggoth IR carries
its hooks**. Each nominal type's `(type …)` declaration lists them in its
pragmas, and that list is the only place shoggoth looks.

This document covers how the list is built in nimsem and how shoggoth reads it.

Implementation points:

- `src/nimony/semdecls.nim`: `registerHook`, `checkCopyDupAgree`,
  `getObjSymId`.
- `src/nimony/semmain.nim`: `requestHookInstance`, which instantiates generic
  hooks.
- `src/nimony/derefs.nim`: `trType` and `addHookDecls`, which embed the
  hooks into the type declaration.
- `src/nimony/programs.nim`: `AttachedOp`, `HooksPerType`, `tryLoadHook`,
  `tryLoadAllHooks`.
- `src/hexer/lifter.nim`: `getHook`, `loadHook`, `requestLifting`,
  `genMissingHooks`.

---

## The representation

The six hooks map to six pragma tags (`AttachedOp` / `HookKind`):

| hook          | `AttachedOp`       | pragma tag   |
|---------------|--------------------|--------------|
| `=destroy`    | `attachedDestroy`  | `(destroy S)`  |
| `=wasMoved`   | `attachedWasMoved` | `(wasmoved S)` |
| `=dup`        | `attachedDup`      | `(dup S)`      |
| `=copy`       | `attachedCopy`     | `(copy S)`     |
| `=sink`       | `attachedSink`     | `(sinkh S)`    |
| `=trace`      | `attachedTrace`    | `(trace S)`    |

The tags sit in the type declaration's pragmas, and `S` is the hook routine's
symbol:

```
(type :Res.0.mod . .
  (pragmas
    (destroy =destroy.0.mod)
    (copy =copy.0.mod))
  (object . (fld :p.0.mod . . (pointer) .)))
```

A type with RTTI (an inheritable object that has methods) also carries a
`(methods (kv "<signature>" S) …)` pragma, which is its vtable. Its `=destroy`
is generated as a method and appears both as `(destroy S)` and as a vtable
entry under `destroyMethodKey()`. Under `--mm:orc` the same applies to
`=trace` with `traceMethodKey()`. A consumer in another module therefore
reuses the owner's dispatched hook instead of forging its own. A forged hook
would not be in the class's vtable.

A missing tag for an operation means one of two things:

- the operation is **trivial** for this type (a bitwise copy, or no-op
  destruction), or
- the type has user hooks for other operations, and shoggoth lifts this one
  field-wise on demand (see below).

## Step 1: sem records hooks per type

When sem checks a routine named `=destroy`, `=copy` and so on, `semHook`
validates the signature (`checkTypeHook`). `registerHook` then records the
routine in `SemContext.typeHooks: Table[SymId, HooksPerType]`. The key is the
symbol of the type that the first parameter names. `getObjSymId` strips
`var`/`sink` and any `(at …)` invocation, so a hook on `Gen[T]` is keyed by the
generic type `Gen`.

`checkCopyDupAgree` rejects a `=copy` and a `=dup` that disagree on `.error`.
They are both copies, and if only one were `.error` the other would let a
forbidden copy through. `.error` hooks are recorded in `errorHooks`, and only
registered when their signature is valid.

At this point the hooks exist only in that table. Code that needs them before
the module is published reads the table directly. For example,
`requestHookInstance`, `hasDestroyHook` in `semmagics.nim` and `derefs`'s own
lifter (through `frontendHooks`) do this for types of the current module. For
imported types they read the published declaration's pragmas with
`tryLoadAllHooks` / `tryLoadHook`.

## Step 2: generic types get instantiated hooks

A generic type's hooks are generic routines, and shoggoth cannot call those.
For every type instance a module creates (`typeInstDecls`),
`requestHookInstance` does the following:

1. It looks up the generic type's hooks. For a generic type declared in this
   module it uses `typeHooks`. For an imported one it reads the `(destroy …)`
   etc. pragmas on that module's generic declaration.
2. It requests a routine instance of each hook, binding the hook's type
   variables to the instance's type arguments.

`instantiateGenericHooks` then semchecks those instances like any other
routine. That runs `registerHook` again, and this time the key is the **instance
type's** symbol. An instance type therefore ends up with concrete,
non-generic hooks, just like a hand-written nominal type.

A generic type's own declaration still lists its generic hooks in its pragmas.
They are the source for step 2 in importing modules. Shoggoth ignores them:
`lifter.loadHook` drops any hook routine that has type parameters.

## Step 3: `derefs` embeds the hooks

`injectDerefs` receives `typeHooks` at the end of nimsem, just before the
module becomes Shoggoth IR. `trType` rewrites every `(type …)` declaration:

- **The type has registered hooks or methods.** `addHookDecls` appends one
  `(op S)` per registered hook to the existing pragmas. For an RTTI type it
  also adds the lifted `=destroy` (and, under ORC, `=trace`) if the user did
  not provide one, and emits `(methods …)`.
- **The type is a non-generic `object`, `distinct` or `ref` with nothing
  registered.** The lifter (`getHook`) runs for each of the six operations. A
  non-trivial operation gets a **forged** hook, synthesized field-wise, and
  its `(op S)` is added. A trivial one gets nothing. `genMissingHooks` then
  appends the forged routines to the module, so they are published with it.
- Otherwise the pragmas are copied unchanged.

After this step the Shoggoth IR is self-describing. Every nominal type that
needs lifetime management names its hooks in its own declaration, and the
named routines are published in the same module or in the one that declares
the hook.

## Step 4: shoggoth reads them back

Shoggoth's passes (`duplifier`, `destroyer`, `desugar`) ask
the lifter in `lengcgen.nim`'s context through `getHook(op, type)` /
`getDestructor(type)`. That context has no `frontendHooks`, so for a nominal
type `loadHook` checks two places:

1. its per-module cache, `nominalTypeToHook`,
2. otherwise `tryLoadHook(op, typeSym)`, which loads the type declaration and
   scans its pragmas for the tag.

When neither gives a hook and the type is not trivial for `op`, or the type
is structural (a tuple, an array, the tuple a closure lowers to, an anonymous
`ref object`, `owned ref`), `requestLifting` synthesizes one. Its name is
`=<op>_<mangled type>.<n>.<module>`, and it is cached in
`structuralTypeToHook`, keyed by the mangled type. A structural type's hooks
are therefore generated per module that needs them, not once globally. The
`tryLoadSym` check in `requestLifting` reuses a hook that was already forged
under the same name.

Two lifter rules depend on the embedded `.error` hooks:

- If a type's `=copy` or `=dup` is `.error`, the lifted sibling becomes
  `.error` too (`hasHook`, `siblingHookError`). Any hook that calls an
  `.error` hook is also `.error` (`maybeCallHook`), so an aggregate with a
  move-only field is itself move-only.
- `owned ref`'s `=copy`/`=dup` is always `.error`.

## Invariants

Shoggoth depends on these. Breaking one leads to silent leaks or double-frees,
not to a compile error:

- **The declaration is the source of truth.** A hook that is not in its
  type's pragmas does not exist for shoggoth, which will lift a field-wise one
  in its place. Every hook sem registers must reach `addHookDecls`.
- **Hooks named in a non-generic type's pragmas are non-generic and
  loadable.** Generic hooks are instantiated per instance type in step 2.
- **`=copy` and `=dup` agree on `.error`.**
- **RTTI `=destroy` (and ORC `=trace`) appear in both places**, as a hook
  pragma and in `(methods)`, under the same symbol.
