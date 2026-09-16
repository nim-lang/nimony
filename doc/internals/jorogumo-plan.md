# JOROGUMO — the NativeNIF JavaScript backend (`nimony j`)

Status: **in progress — M0…M3 done, M5 partly** (structured control flow and `lab`/`jmp`). The
generator produces real JavaScript: `188 / 243` of the arkham fixtures generate and `186 / 186` of
those behave identically to the native backend under node. What still stands between here and
"hello world prints under `nimony j`" is **M4, module linking** — a whole-program `.js` needs the
foreign-module data and the `ini` chain, which the fixture corpus (single-module, hand-written Leng)
does not exercise. See *M3 L1+L2 results* for the state of the code and *Next* for the order.
Tracked upstream: [nim-lang/nimony#2445](https://github.com/nim-lang/nimony/issues/2445).
Twin generator: **ithaqua** (`../nativenif/src/ithaqua`, `doc/ithaqua.md`). The rule from #2445 §8
applies throughout: *follow ithaqua, do not re-invent it*. This document is the build plan; on
landing it is superseded by `nativenif/doc/jorogumo.md` (the final reference, mirroring `ithaqua.md`).

## Name

**jorogumo** — after the [Jōrōgumo](https://en.wikipedia.org/wiki/Jor%C5%8Dgumo), the spider yōkai
that weaves a web and wears a borrowed form: a weaver of webs (the DOM *is* a web) and a shapeshifter
(the interop boundary), which is exactly this backend's two defining skills. Chosen with Araq's
blessing. Tool `src/jorogumo/jorogumo.nim`, driven by `nimony j` (§7 of the issue mandates the
command form — not a plugin, not a `lengc js`).

## Shape (file for file, against ithaqua)

| ithaqua | jorogumo | role |
|---|---|---|
| `ithaqua.nim` | `jorogumo.nim` | CLI: getopt, `parseFromFile` with shared tag pool, run |
| `wasmenc.nim` | `jsenc.nim` | tiny jsnif → JS **text** emitter: precedence/parens, no logic |
| — | `jsnif.nim` | the `JsTag` enum + tree builder (§3 — wasm needs no tree, JS does) |
| `codegen_wasm.nim` | `codegen_js.nim` | the bulk: whole-program Leng → jsnif |
| `twasmenc.nim` | `tjsenc.nim` | golden unit tests, no dependencies |

Reuses arkham's language-neutral program model — `core/programs.nim` (lazy foreign-module
loader), `core/typenav.nim`, `core/asmslots.nim` — plus nimony's NIF reader libraries, exactly as
ithaqua does. `setTargetWord(Wasm32)` is called once **before anything is parsed** so the layout
answers and the generator's own pointer arithmetic agree; an aggregate laid out under a
disagreement is not detectably wrong later, just a field at the wrong offset.

Whole-program emission: start at the entry proc plus every `exportc`, pull in what is reachable,
no per-module output, no link step (§7, §8).

## jsnif (§3)

The generator emits a NIF tree with a dedicated JS tag enum, not strings. The tag pool is seeded
so `TagId == ord(JsTag)` **by construction** — the `lengdecl.nim` pattern
(`nativenif/src/arkham/core/lengdecl.nim:24`: register tags in master order, `assert` alignment,
decode with accessor templates). No `ids: array[JsTag, TagId]` side tables.

The tag set covers at minimum: `function`, `var`, `return`, labeled `block`, `break`,
`if`/`else`, `while`, expression-statement, binary/unary operators, call, property access,
index, `new`, typed-array views, string/number literals, and — from day one, because the DOM
requirement puts interop in the core — the **extern-value ops**: handle wrap/unwrap, jsstring
literal, raw-JS splice (the `importjs` mechanism), 1-sized-array cells.

`jsenc` is then trivial enough to audit by eye. A peephole pass over jsnif can come later (§3).

## Memory model (§1, §8)

One `ArrayBuffer` as linear memory; a pointer is an integer offset into it; `HEAP8/HEAP16/HEAP32/
HEAPU*/HEAPF64` are its views. All low-level code, including `cast`, works on bytes — "JS is
dynamically typed" is not a memory model (§1).

- Gvars at fixed linear-memory addresses; `errv`/`ovf` are JS globals outside the buffer, as they
  are wasm globals in ithaqua.
- Constant aggregates (string literals included) become data blobs with absolute-address fixups
  applied at startup — jorogumo owns the final layout, there are no relocations.
- Address-taken and aggregate locals live on a **shadow stack** in linear memory behind an `sp`
  global; aggregate params are passed as address-of-a-fresh-copy; aggregate results via hidden
  first parameter (sret). All carried wholesale from ithaqua (§8).
- **Memory growth**: the `wasm32+standalone` osalloc arm (`nimony/lib/std/system/osalloc.nim:200`)
  already bump-allocates over linear memory via `__builtin_wasm_memory_size` /
  `__builtin_wasm_memory_grow`. jorogumo resolves both as host functions in the shim over a
  **pre-sized, non-growable** ArrayBuffer: `grow` is a capacity check (returns −1 past the end,
  the wasm contract). Copy-and-rebind growth can come later and matches `memory.grow` semantics
  exactly, including the stale-view hazard. Default buffer size is host-chosen (a JS ArrayBuffer
  commits real memory, so "reserve 4 GiB" is not free; start ~64–256 MB, host-overridable).
- The native allocator is compiled to JS as-is (§5) — it rides the osalloc arm above; no
  `mi_malloc` shims.

## The addr/deref ruling (§2 vs §8)

§2 forbids mapping `(addr x)`/`(deref x)` to plain `x` — the origin of most complexity in the old
JS generator — and prescribes 1-sized arrays. §8 says ithaqua's shadow-stack decision transfers
wholesale. Ruling, reconciling both:

- An address-taken **Nim** local becomes a shadow-stack slot: `addr x` yields an integer address,
  reads become `HEAP*` accesses. One representation, identical to wasm, no dual bookkeeping.
- The 1-sized array (`addr x` → `x`, `x` → `x[0]`) is kept exactly where §2's review produced it
  and §6 needs it: an address-taken **JS value** (a `var` param carrying a JS object — the fat
  pointer case). Its cell is a 1-sized JS array holding one handle-table slot.

The invariant that survives in both: `addr`/`deref` always denote real memory, never identity.

## Control flow (§4)

Hexer's `jmp`/`lab` lower to **labeled blocks + `break`** — no relooper, because `jmp` is
forward-only and scoped (it may leave enclosing constructs, never enter one). Constructs that do
not fit the grammar are *rejected loudly*, not partially emitted. `ite`/`case`/loops are the
structured forms. Exception landing pads nest label/pad blocks in reverse close-event order.

## The JS value bridge — interop and DOM are core, not a follow-up

Good DOM support is a launch requirement, so #2445 §6 moves from "later" into the build. Anything
that must be a real JS value lives **outside** the ArrayBuffer:

- **Handle table.** JS objects and JS strings live in a host-side table; the Nim side holds an
  opaque `int32` handle — 4 bytes, storable in aggregates, passable through calls, comparable.
  `DOMObject` is a handle; `jsstring` is a handle to a real JS string.
- **`importjs` bridging.** An `importjs` proc splices raw JS (`#.method(#)`); operands typed as
  handles are unwrapped at the boundary and return values wrapped back. The template language must
  be Nim 2's exactly — `#` operand, `$1` nth-param-name, `$$` escape, `@` varargs spread — so the
  existing binding declarations port verbatim (see "Prior art" below). Requires the pragma to
  survive nimsem verbatim (§6 claims it does — verified in the spike; if not, that is
  first-priority nimony-side work).
- **`jsstring` ↔ `string`** conversion bridges UTF-8 bytes in linear memory to JS's string type.
- **Callbacks.** Passing a Nim proc to a JS API (`addEventListener`) needs a Nim→JS callable:
  emit one JS closure per proc address that marshals args and calls the generated function —
  the funcref-table analogue from ithaqua's static function-pointer story.
- **Liveness.** Handles are GC roots while held. First cut: the table never releases (leak-tolerant,
  documented); proper handle lifetime is a tracked follow-up.
- **`lib/std/dom.nim`**: bindings written against the handle model, but **source-compatible with
  Nim 2's `std/dom` usage** — port `lib/js/dom.nim`'s declarations (they are mostly pragma
  templates, see "Prior art"), retyped so `importc ref object` means "handle" instead of "native
  JS object". Code written against Nim 2's dom API should compile with little more than the import
  path changing. `jsffi`-style dynamic objects (`.()` operator, `JsObject`) are out of scope for
  the first landing.

## Prior art: what Nim 2's JS backend teaches (surveyed in Nim 2.2.4)

**Adopt — the user-facing binding surface, so existing code and bindings keep working:**

- `importjs` template language: `#` operand, `$1` nth-param-name (`"#.$1(#, #)"`), `$$` escape
  (`"$$(#)"`), `@` varargs spread (`"#.$1(@)"` with `{.varargs.}`) — `lib/js/dom.nim:1783+`,
  `lib/js/jsffi.nim`, `lib/js/asyncjs.nim:79` (`PromiseJs {.importjs: "Promise".}`).
- `importcpp` doubles as the JS member/method template (`"#.id"`, `"#.childNodes[#]"`) — dom.nim
  uses it pervasively (`lib/js/dom.nim:1379+`); jorogumo must accept it on the JS target too.
  `nodecl` elides the declaration; `importc` on a name maps to a plain JS identifier
  (`window`, `document` at `dom.nim:1695,1700`).
- `{.importc.} ref object` as the opaque external-type idiom; `cstring` at the boundary is the
  JS-string idiom — both keep their signatures, remap to handles/`jsstring`.
- Callback idiom: `proc (event: Event) {.closure.}` fields on `EventTarget` +
  `addEventListener(ev, cb)` + `setTimeout(cb, ms)` — exactly the M7 wrapper-closure workload.
- dom.nim's `when defined(nodejs)` **dummy-DOM fallback** (`dom.nim:1393+`) is the prior art for
  testing DOM code without a browser — jorogumo does it better with jsdom rather than a
  hand-rolled fake.
- `lib/js/asyncjs.nim` Promise↔`Future` bridging: the pattern to follow later for `fetch`/async
  APIs; not first landing.

**Reject — jsgen's codegen model, which is precisely what #2445 §1/§2 closes the door on**
(`compiler/jsgen.nim:187` `mapType`): every `ref`/`object`/`seq` becomes a native JS value
(`etyObject`), pointers become JS references (`etyBaseIndex`), `tyLent` is a no-op "as JS has
pass-by-reference semantics", sets map to JS tables — no stable layout, `cast`-unsafe, and the
source of the addr/deref mess. None of that crosses into jorogumo; only the pragma surface does.

## Driver changes (nimony repo, following `backendWasm` step for step)

- `backendJs` in `src/nimony/nifconfig.nim:90`, next to `backendNative`/`backendWasm`.
- `of "j":` in `src/nimony/nimony.nim:144` → `config.backend = backendJs`, `FullProject`.
- Target implication **shared with the wasm block** at `nimony.nim:354`
  (`backend in {backendWasm, backendJs}`): appended to `commandLineArgs` *after* CLI parsing so
  nimsem sees it — `--cpu:wasm32 --os:standalone --bits:32`. `standalone`, not `embedded`: it
  routes `syncio` into the raw `write`/`read`/`open` arm whose names the host shim resolves.
- `src/nimony/deps.nim`, beside the `wasm` bool (`:1096`): the whole-program shape — no
  per-module codegen step, `findTool("jorogumo")` invoked on the MAIN module only, output named by
  the `wasmFile` analogue with a `.js` ext (`:126`, `:2225`), and `-r` running the host shim
  (`:2265`) — `tests/jorogumo/run_js.js`, mirroring `tests/ithaqua/run_wasm.js` but resolving
  `nim_write`/`nim_exit` **and** the `memory_size`/`memory_grow` pair.
- `buildJorogumo` in `src/hastur/builders.nim`, opt-in like `buildIthaqua` (`:112`).

## Milestones

Written as the work landed, so some bullets describe a gap a later milestone closed;
the status of record is what `tjsgen` prints, and *Next* is the honest to-do list.

- **M0 — spike.** Hello-world through `nimony w`; inventory the Leng tag surface ithaqua handles
  (that list *is* the instruction-selection checklist); confirm forward-only `jmp` in `.c.nif`;
  check `importjs` survival through nimsem — including the `#`/`$1`/`$$`/`@` template arguments
  the Nim 2 bindings rely on; settle the ArrayBuffer default size; lock the §2/§8 ruling above.
- **M1 — jsnif + jsenc.** Tag enum, builder, printer, `tjsenc.nim` goldens. Extern-value ops are
  in the tag set from day one.
- **M2 — driver with a stub tool.** `nimony j` end to end: pipeline runs, stub `jorogumo` emits
  `console.log("ok")`, `nimony j -r` prints under node. This buys the fixture loop for everything
  after.
- **M3 — codegen skeleton.** Program model, `setTargetWord`, gvars, data blobs + fixups, HEAP
  views, `nim_write`. Hello world prints real output under node. **M3a done** (layout, gvars, data +
  fixups, the loader and host face — see *M3a results*); its exit criterion needs M4+M5's codegen,
  per the measurement there.
- **M4 — locals, calls, shadow stack.** `sp`, address-taken/aggregate locals, sret, fresh-copy
  aggregate params, proc table, closure bridge thunks. **Done in substance** by M3 L2 (see the
  results below) for one module; what is missing is the multi-module half — foreign-module data,
  the `ini` chain, indirect calls through the function table, `{.importc.}` procs that carry a body.
- **M5 — control flow + EH.** Structured forms (`if`/`while`/`break`/`case`) and `jmp`/`lab` are
  **done**; try/catch landing pads in reverse close-event order are not, and are what a real
  `nimony j` program needs beyond M4.
- **M6 — allocator + strings.** osalloc wasm32+standalone arm over the host `memory_size`/`grow`;
  Nim strings as linear-memory data. At this point non-trivial programs should pass `jsdiff`.
- **M7 — the JS value bridge.** Handle table, `jsstring`, `importjs`, fat-pointer `var` params,
  callback wrappers.
  - **M7 gate (nimony side) — DONE.** `{.importjs.}` now survives the whole front half to jorogumo's
    input. nifler already passed it verbatim (a generic `(kv importjs~ "…")` pragma node, no nifler
    change needed); nimsem resolves it to a new `ImportjsTagId` — a row appended to the end of
    `doc/tags.md`, which is the master `TagData` nativenif builds its Leng tag pool from, so the tag's
    *number is that row's position* and nativenif must be rebuilt against the same `doc/tags.md`
    (`createLengTagPool` asserts every id) — and emits `(importjs "template")` in the `.s.nif`;
    hexer forwards it into the Leng `.c.nif` (a new `ImportjsP` in `leng_tags`, an `externKind`
    branch in `lengcgen`, and bodyless-importjs treated as a foreign proc in `funcsummary`/
    `intramodinliner` so its empty `(stmts .)` body is neither analysed nor inlined). The other back
    ends have no lowering for it and say so: `lengc` rejects the pragma outright instead of emitting a
    call to nothing. jorogumo reads the tag with the template byte-intact and, until the splice is
    implemented below, **refuses at the call site** (`extern … (the JS bridge is M7)`) rather than
    emitting a silent empty stub. Remaining for M7: lower the `#`/`$1`/`$$` splice template to a JS
    call and the handle/`jsstring` bridge.
  - **M7 splice + handle bridge — DONE.** A bodyless `importjs` call lowers to a JS splice: jorogumo
    emits a `Raw` node `(raw NAME "tpl" ARG…)` and jsenc's `spliceTemplate` substitutes the operands
    (`#`/`$1`/`$#`/`@`/`$$`, pinned to Nim 2.2.4). `$1`/`$#` splice the **Nim source name** of the
    proc, not the Leng symbol: `extsym` carries `insertAdjacentText.0.dom`, and the template needs
    `insertAdjacentText`, because Nim resolves `%name` at declaration time, before mangling. The
    base is the text up to the first `.` and a base that is not an identifier is a refusal rather
    than pasted JS — Nim would not have produced a parseable template either. So `#.$1(#)` and
    `#.$#()`, the method idioms `dom.nim` is built on, generate; a name whose Nim spelling is not a
    JS identifier is refused instead of emitted as a SyntaxError waiting to happen.
    A pointer/`ref object` **operand** of the splice is
    unwrapped (`eunwrap`), a pointer/`ref` **result** is wrapped (`ewrap`) — a JS handle is an int32
    into the host table `JSP`, confined to splices; ordinary Nim pointers stay real addresses. In
    library mode the surface exports `__internExt: ewrap` so a host interns real JS objects (a
    `GPUDevice`, a canvas context) as handles. Proven under node with a mock `navigator.gpu`: a host
    interns a device, the module drives `createBuffer`/`createCommandEncoder`/`queue.submit` over it.
  - **M7 jsstring — DONE.** A Nim `string`/`cstring` now crosses an `importjs` splice both ways. The
    type is classified by `jsBridgeKind` in precedence order (string → cstring → handle → scalar):
    a Nim `string` is the `string.0.<system>` symbol (Leng has no builtin kind for it), a `cstring` is
    `(aptr char)` — which `isPtrType` would otherwise misclassify as a handle, hence the order. The
    bridge mirrors `lib/std/system/stringimpl.nim`'s SSO layout for the 4-byte target (byte0=slen;
    slen ≤ `PayloadSize`=6 keeps chars inline at `value+1`, else `more`@`value+4` → `LongString`
    {fullLen@0, rc@4, capImpl@8, data@12}): `nimStrToJs`/`cstrToJs` decode an operand (read-only, the
    same logic as `len`/`rawData`); `jsToNimStr` builds a **static** string (byte0=254, capImpl=0 —
    the GC's `=destroy` frees only a HeapSlen string, so a result is never freed/refcounted, leaking
    rather than risking a bad refcount, matching the never-released handle table) and syncs the 3-char
    inline cache so `==`/`hash` agree with a compiler literal; `jsToCstr` NUL-terminates. Verified for
    short/long/static strings, `==` against literals, byte lengths, and multi-byte UTF-8 + emoji
    surrogate pairs in both directions, in node and browser modes.
  - **Callback bridge (Nim proc → JS callable) — DONE.** A `proc(...)`/`closure` operand of an
    `importjs` splice is classified `jbCallback` (proctype checked before the handle case, since a
    proc value is also a pointer). When the signature is all-scalar with a void/scalar result the
    Nim proc is already JS-callable — jorogumo emits procs as ordinary JS functions with no env
    arg — so the splice receives the function-table entry `FTAB[i]` directly; otherwise jorogumo
    emits a generated wrapper `function __cbN(...a){ … }` that bridges each parameter by kind
    (`jsToNimStr`/`jsToCstr`/`ewrap`) and the result back (`nimStrToJs`/`cstrToJs`/`eunwrap`). The
    rest-args form lets the host pass more than declared (rAF's timestamp). A bridged wrapper must
    name a Nim proc directly — a stored fn-ptr or capturing closure is refused (jorogumo carries
    no closure env), as are aggregate parameters and a splice that would RETURN a callback.
    Verified: `requestAnimationFrame(frame)` fast path under a browser-less vm with a mock rAF
    (frame counter advances with the timestamp ignored), and a `proc(msg: string; h: pointer)`
    wrapper with UTF-8+emoji text (`echo` via an exported `flushFile(stdout)` — library mode never
    flushes by itself) and full JS-object identity through `ewrap`/`eunwrap`.
  - **Browser target (`--browser` / `--target:browser`) — DONE.** A second host face for the JS
    backend. `jorogumo --target:browser` (and `nimony j --browser`) drops the Node `fs`/`process`
    contract from the preamble: `nim_write` sends complete lines of fd 1/fd 2 to `console.log`/
    `console.error` as they form (per-fd streaming `TextDecoder`, so a UTF-8 sequence split across
    flushes survives; the unterminated tail waits in a per-fd pending buffer), `__takeOutput()`
    drains that tail plus any write to an fd the console cannot serve, `nim_exit` throws instead
    of killing the tab, and the export surface lands on
    `globalThis.NIF` (not `module.exports`), so a classic `<script>` and a module host both find it.
    node/CommonJS stays the default — the node branch of the preamble is byte-identical to before
    (jsdiff 24/24, tjsgen 229/231 unchanged). The flag is threaded `nimony j` → `NifConfig.jsBrowser`
    → the emitted jorogumo command (`deps.nim`), materialised into the `.build.nif` so child
    processes need not re-derive it. A full clear-canvas frame (getCurrentTexture().createView →
    createCommandEncoder → beginRenderPass{colorAttachments:[{view,loadOp:"clear",clearValue,
    storeOp:"store"}]} → end → queue.submit([finish()])) drives under a browser-simulating `vm`
    context (no `require`/`module`/`process`/`Buffer` present) with a mock WebGPU, and a served
    `index.html` host owns the async `requestAdapter`/`requestDevice`/`configure` + the `rAF` loop.
- **M8 — DOM.** `lib/std/dom.nim` ported from Nim 2's `lib/js/dom.nim` declarations — handle-typed,
  source-compatible usage — plus DOM fixtures under jsdom.
- **M9 — verification + docs.** `hastur jsdiff` over the full fixture set; `jorogumoTests` in
  nativenif; promote this plan to `nativenif/doc/jorogumo.md`; bump the `src/nativenif.commit` pin.

## Verification

- **`hastur jsdiff`** — clone of `src/hastur/wasmdiff.nim`: every fixture in `tests/jorogumo/*.nim`
  (seeded from `tests/ithaqua/`, each `.nojoin`) runs through the native backend as the executable
  oracle and through `nimony j -r` under node; stdout must be byte-identical, exit codes must
  match. The oracle cuts both ways — it has caught native-backend bugs before.
- **`jorogumoTests`** in `nativenif/tests/tester.nim` — every `tests/arkham/*.c.nif` fixture through
  the generator, requiring a `.js` out of each that is not on an explicit refusal list. It runs
  nothing; it exists so a rename or signature change in `core/` cannot silently break the
  out-of-tree consumer (that is how ithaqua's first merge broke).
- **DOM fixtures** run under node + `jsdom` (dev dependency under `tests/jorogumo/`). jsdom is a
  smoke harness, not the spec truth — real-browser runs are out of scope for CI here.
- `tjsenc.nim` goldens cover the emitter with no dependencies at all.

## Risks

1. **EH lowering** is the historically hardest half in both backends; budget M5 accordingly.
2. **`importjs` passthrough** — **resolved.** Verified end to end: the template survives nifler →
   nimsem (`(importjs "…")` tag) → hexer (forwarded into the Leng `.c.nif`) → jorogumo byte-intact.
   See the *M7 gate* note under Milestones.
3. **Handle liveness vs GC** — the never-release table is a deliberate, documented leak until the
   ARC story extends across the bridge.
4. **ArrayBuffer sizing** — JS commits real memory up front; too big breaks low-end hosts, too
   small breaks big heaps. Host-overridable from day one.
5. jsdom ≠ browser: DOM tests prove the bridge, not rendering fidelity.

## M0 results (spike, done)

1. **Pipeline**: `nimony w` hello-world prints under node via `tests/ithaqua/run_wasm.js`.
   Note the target's stdlib face: plain `echo` is undeclared on `standalone` — fixtures import
   `std/syncio`, and that is the arm whose `write` names the host resolves. Same for jorogumo.
2. **Forward-only `jmp` holds**: 1552 `jmp`s across 153 Leng modules (all 25 `tests/ithaqua`
   fixtures + hello), **zero** backward or orphaned. The labeled-block lowering (§4) is safe to
   build on. (Scan caveat: `lab` rows carry `@info` suffixes and `:`-sigiled names — `lab@,1 :L`.)
3. **Instruction-selection checklist** (ithaqua's coverage, which jorogumo mirrors):
   `LengExpr` 44/45 (only `OffsetofC` unhandled), `LengStmt` 31/31, `LengType` 9/16 by tag — the
   scalar/array types are answered through typenav/layout sizes, not tag dispatch. Pragmas are
   read by *string inspection* (`codegen_wasm.nim:321 importcExternOf`), not the pragma enum.
4. **Interop gate (the M0 headline)**: `importjs` does not exist in nimony — no tag, no sem; a
   source-level `{.importjs.}` fails with "expected pragma" (`sempragmas.nim:200`). The stdlib's
   references are `defined(nodejs)` arms nimony never activates. Custom `{.pragma.}` templates DO
   survive nimsem verbatim as `(pragma sym "args")` (verified in `.s.nif`), but hexer's
   `externPragmas` whitelist (`lengcgen.nim:163-200`) drops them before Leng — and jorogumo reads
   Leng. §6's "pragmas verbatim" holds for *plugins on the sym IR*, not for this path.
   **Required nimony PR, small and well-scoped**: `ImportjsTagId` in `doc/tags.md` + `gen_tags`
   regen → `ImportjsP` in `LengPragma` → `sempragmas` accepts it with the string arg → hexer
   `externKind` gains the `"importjs"` branch. `importcpp`/`nodecl`/`header` already pass through.
   This gates M7/M8 — do it right after the driver lands.
5. **Pre-existing bug found (not ours, worth reporting)**: a bodyless proc without
   `importc`/magic, *called*, is silently accepted and evaluates to the default value —
   `echo mystery()` prints `0`, exit 0. No "not implemented" error anywhere in the pipeline.
6. **Decisions locked**: ArrayBuffer default **64 MiB**, host-overridable; `memory.grow` is a
   capacity check past it (clean `raiseOutOfMem`, wasm contract). §2/§8 ruling locked as written
   above. jsdom not installed — M8 item.

## M1 results (`jsnif` + `jsenc` + goldens, done)

`nativenif/src/jorogumo/`: `jsnif.nim` (168 lines, tag pool + builders), `jsenc.nim` (556, the
emitter), `tjsenc.nim` (365, 19 goldens + 2 engine-judged programs). `nim c -r tjsenc.nim` is
green; the engine checks run real generated JS under node — one prints `hello jorogumo` through
the bridge, one computes `42` through the `ArrayBuffer` heap.

Four things §3 did not survive contact with the tree, all simplifications:

1. **Literals are plain NIF tokens.** `IntLit`/`FloatLit`/`StrLit` carry the numeric and string
   literals, as in every other NIF dialect, and a raw JS name is nifcore's `Ident` *token*
   (`addIdent`) — "an identifier that is not a Nim symbol", which is exactly what a host global
   or an `importjs` name is. `JsTag` therefore has no `Num`/`FloatLit`/`StrLit`/`Ident`; only
   `BigIntLit` (a u64 does not fit an `IntLit`) and the keyword literals need tags. The emitter
   dispatches on the token kind first, on tags second.
2. **`indent` is threaded through `exprText`, not just `stmtText`.** An `arrow` body is a
   statement list inside an expression; without the level the body and its closing brace land at
   column 0 of a block that is not theirs.
3. **`escapeJsString` keeps text as text.** Nim strings are UTF-8 and a JS source file is UTF-8,
   so well-formed sequences pass through; only bytes that cannot start one (lone continuations,
   overlongs, surrogates) and control bytes go out as `\xNN`, and the `U+2028`/`U+2029` sequences
   as `\u202X`. Byte-exact data does not ride on string literals — it is a data blob (§M3).
4. **`execCmdEx` re-adds a trailing newline** the child did not write, so the engine checks
   compare stripped output.

`Raw` is `(raw NAME TPL ARG*)` with the template semantics pinned empirically against Nim
2.2.4's jsgen (`#` consumes the next argument, `$1`/`$#` name the proc, `@` spreads what is
left, `$$` is a literal `$`, a bare `$` is an error). `dom splice`, `escaped dollar` and
`varargs spread` are the three goldens that pin it.

## M2 results (`nimony j` driver with the stub tool, done)

`nimony j hello.nim` builds and `nimony j -r hello.nim` prints under node. The stub `jorogumo`
parses the Leng `.c.nif` it is handed (so a broken pipeline fails in the tool, not silently) and
emits the preamble plus a fixed `console.log("ok")`.

Driver, in the order the wasm arm was mirrored:

| site | change |
|---|---|
| `gear2/modnames.nim` | `BackendDirJs = ".js"` |
| `nifconfig.nim` | `backendJs = "js"` |
| `nimony.nim` | `of "j"` arm; the implied-target block is now `backend in {backendWasm, backendJs}` — one 32-bit freestanding target serves both, and `wasm32` is what selects osalloc's memory-growth arm (JS borrows the selection, it does not run wasm) |
| `deps.nim` | `jsFile` next to `wasmFile`; `genFile`/`backendDirName` arms; **`wholeProgram = wasm or js`** replacing the `wasm`-only branches — one tool is codegen and linker, exactly one node, no per-module codegen, no objects, no link step; `-r` runs `node <out.js>` |
| `hastur/builders.nim` | `buildJorogumo`, plus the `build jorogumo` spelling and its usage line |

Two things worth keeping in mind:

1. **JS needs no run shim.** wasm needs `tests/ithaqua/run_wasm.js` to supply the `env` import
   object; a `.js` program is already host-native — the preamble *is* its host face. So `-r` is
   `node out.js`, arguments forwarded as-is.
2. **A pre-existing cache bug surfaced while testing (not ours, verified against pristine HEAD),
   now filed:** building the same `nimcache/` first with `nimony w` and then with `nimony c` links
   the wasm-targeted `osalloc` object into the native executable — `undefined reference to
   __builtin_wasm_memory_size`. Nothing upstream re-runs: nimsem is invoked 0 times on the second
   build even though the config memo changed and `--rerun` was passed. Repro, evidence and the
   discriminating table: same-target pairs are fine, every target-changing pair breaks.
   Workaround: `-f`. Until it is fixed, a fixture
   run must not share a `nimcache/` with another target.

Regression after the driver landed: **797/797** `hastur tests` (whole tree, `HASTUR_SKIP=tests/boot`)
and **25/25** `hastur wasmdiff`.

## M3a results (the layer that owns linear memory, done)

`codegen_js.nim` (480 L): the layout half of the generator, ported step for step from
`ithaqua/codegen_wasm.nim` — `allocStatic`/`globalAddrOf`/`strLitAddr`/`flexPayloadLen`,
`fieldOffsetIn`/`dotOffset`, `constScalarBits`, `serializeConstInto`, the static-vs-runtime
initializer ruling, and the finalize-time segment-overrun invariant. It is emission-free: what
comes out is a `memTop`, one address per global and `(address, bytes)` segments.

The image lands in JS as one `D(base64, address)` call per segment — the data section's twin;
base64 because the image is arbitrary bytes and a JS string literal is not. The preamble gained
that loader plus `nim_write`/`nim_exit`, the same two entry points ithaqua imports from `env`
(node-only by nature; M8 gives DOM programs a console-backed one).

`tcodegen.nim` + `fixtures/tdata1.c.nif`: 19 checks — field offsets through an inheritance-free
object with a flexarray tail, array element stride, a proc symbol resolving to a function-table
slot, a nil slot, a zero-init gvar reserving space without a segment, an i64 initializer surviving
the little-endian writer — and an **engine check**: node reads the string back *through the stored
`(addr g)` fixup*, so linear memory, the fixup and the loader are judged by a JS engine, not by the
text that produced them.

Two things that cost time and are worth remembering:

1. **A buffer is Leng-tagged or JS-tagged, never both.** Tag ids are resolved through the buffer's
   own `TagPool`, so parsing `.c.nif` with `createJsTagPool()` silently returns `NoType`/`stmtKind 0`
   instead of the Leng tags. `createLengTagPool()` (arkham's, not nifcdecl's) for every `.c.nif`.
2. Fixture grammar has empty slots that are easy to omit: `(type :name PRAGMAS BODY)` and
   `(object BASE FLD*)` — dropping the `.` base makes the first `(fld …)` the base class.

**Scope of the rest of M3, measured.** The hello-world `.c.nif` closure is 142 procs / 417 calls /
602 derefs over the whole Leng op set, and a program cannot run on a partially covered generator —
so "hello world prints real output" is really "M3+M4+M5 together" (`codegen_wasm.nim` is 3136 L on
that count). The generator is being built in that order; `jorogumo.nim` stays on the M2 stub until
it covers bodies, so the tree and `nimony j -r` stay green throughout.

## M3 L1+L2 results (bodies, frames, addressing — done)

`codegen_js.nim` is now ~1950 L and generates whole proc bodies: expressions, statements, calls,
frames, addressing, constructors, sret. Measured by `src/jorogumo/tjsgen.nim`, which runs every
`nativenif/tests/arkham/*.c.nif` through the tool as a *process* and runs the emitted `.js` under
node against the fixture's `.exitcode`/`.output`:

```
188 / 243 fixtures generate; 186 / 188 agree with the native backend under node
55 refused, of which 10 are `err_` fixtures the native back end rejects too
```

The two that generate although the native back end rejects them are `err_ptr_arith` and
`err_aptr_arith`: `(add (ptr T) p q)` is well defined in JS (a pointer is a Number offset) and the
back end sees no reason to be stricter than the language. Rejecting pointer arithmetic on a
non-aggregate is `sem`'s job; the harness names the divergence rather than hiding it.

The remaining refusals are all real feature gaps, each mapped to a milestone: `instr` (13),
`keepovf` (3), the host bridge `memcpy/memmove/memset/memcmp/ulock` (9, M7), foreign-module data
(5) and indirect calls (2) plus `{.importc.}` procs with a body and library fixtures with no
`main` (9, M4), `mmap`/`futex` (2), `{.assembler.}`/`{.naked.}` (3). A refusal is a
`JsGenError` — one line on stderr, exit 1 — never an assertion, and never plausible-looking code.

**Frames.** Every proc gets a shadow-stack frame inside `JMEM`: `frame(n)` moves `SP` down (rounded
to 16) and `leave(f)` restores the base *absolutely*, because the frame aligned down and adding the
size back would not return to it. Address-taken locals and aggregates live in frame slots; plain
scalars are JS `let`s; aggregate parameters arrive as pointers (`lkPtr`); an address-taken scalar
parameter is spilled in the prologue. A struct-returning proc takes a hidden first JS parameter —
the caller's slot — so the result outlives the callee's `leave`.

**The temporary plan.** `planFrame`/`planNode` walk the body pre-order and reserve a slot for every
node that must be materialized (a constructor in value position, every call with an aggregate
result); codegen consumes that sequence through `takeTemp`, which *checks the size* — so a
divergence between the two walks is a refusal naming both sizes, not memory corruption.

**Grammar facts learned the expensive way** (all verified against `doc/internals/leng-spec.md` or
the corpus; several contradict a first reading):

1. Comparisons carry **no type child** — `(lt Expr Expr)`; the width comes from the left operand.
   `and`/`or`/`not` are typeless too, but `ArithExpr` and `shr/shl/bitand/bitor/bitxor` carry one.
2. `dot`'s selector may be an **index or the field symbol**, and a field may carry a trailing
   inheritance depth; only the name form reaches an inherited field.
3. `oconstr` may open with an **inheritance header** instead of a `kv`: the vtable pointer stored at
   offset 0 (ithaqua's rule), or a nested `oconstr` that fills the base subobject **in place** there.
   `inherit_oconstr`'s expected 42 only comes out if the two are told apart.
4. `baseobj` (`(baseobj BASETYPE DEPTH X)`) re-types an object as one of its bases; the base
   subobject is at offset 0, so the address does not move. typenav has no case for it — the type
   must be read off the node.
5. A `conv`/`cast` between two aggregates is a **record conversion**: the value, which for an
   aggregate IS its address, does not move either.
6. `case` lowers to an **if/else-if chain**, not a JS `switch`: a `switch` would capture an inner
   `(break)` meant for the enclosing loop and cannot express `(range LO HI)`. The scrutinee is bound
   once, to a planned temporary.
7. `inc` on a subtree **enters** it (`skip` advances past it), `symName`/`intVal`/`strVal` take a
   value and do not advance, and `into` asserts that every child was consumed. A subtree token is
   `TagLit` — there is no `Tree` nifcore kind.
8. `prog.intType` is the *natural* int — i32 under `setTargetWord(Wasm32)` — so it must never be
   trusted for a 64-bit literal; `(suf LIT "u64")` (and `"+u32"`, with the leading sign) carries
   the real width.
9. Stack slots use arkham's `stackSlotAlign`, not the natural align: an array of ≥16 bytes is
   16-aligned on the stack and 8-aligned in a struct.

**`lab`/`jmp` (§4).** A statement list pre-scans its own `(lab L)` children and wraps itself in one
JS labeled block per label; `jmp L` becomes `break L`, and the `(lab L)` statement *closes* that
block, so the `break` resumes exactly at the label — the same shape ithaqua builds from `block`/`br`,
but wasm needs its blocks nested in reverse close order because `end` is positional while a JS label
is named. Two refusals keep it honest: closing a label that is not the innermost open one, and a
`jmp` whose label does not enclose the jump site. A hexer landing pad (`(if (elif (false) (stmts
(lab L) …)))`) therefore refuses rather than mis-lowering — `jmp L` from the try body reaches a label
that does not enclose it. That is the M5 EH work left to do.

**Locals are declared in the prologue, not at the `(var)` statement.** JS `let` is block-scoped;
wasm locals are function-scoped. Once a `(lab)` wraps a statement list in a labeled block, a `let`
declared inside it is out of sight the moment the `break` lands — a `ReferenceError`, which is the
worst kind of codegen bug because it is silent until runtime. So `lowerProc` emits one `let` per
`lkReg` local up front (zero-initialized in the right world: `0n` for 64-bit, `0` otherwise) and the
`(var)` statement becomes an assignment. Frame slots and the shadow stack are unaffected.

**Running it.** Build the tool with `nim c --outdir:bin src/jorogumo/jorogumo.nim` in `nativenif`;
`nim c a.nim b.nim` in one call is an error, so it is one file per invocation. The driver uses
**`nimony/bin/jorogumo`**, a build product: `hastur build jorogumo` refreshes it, and a stale copy
runs the previous generator silently. `tjsgen` has the same trap in its own directory — it runs
`getAppDir()/jorogumo`, i.e. `src/jorogumo/bin/jorogumo`, which `--outdir:bin` from the repo root
does not touch, so a root-level build leaves the harness measuring the old generator.
`src/jorogumo/tjsgen.nim` is the coverage harness — it runs every arkham fixture as a *process*
(an arkham assertion is fatal, and exit status is the answer,
exactly as `tests/tester.nim` treats ithaqua) and reports generation, agreement and refusals by
cause. `--only:STEM` narrows it to one fixture.

## Next

Status of record, measured on nativenif's `js-backend` branch (rebased onto its `master`; this is
not what `src/nativenif.commit` pins, which predates the tool): `tjsgen` reports **250 / 281
fixtures generate, 250 / 250 agree** with the native back end under node, **30 refused**, and
nothing that the native back end rejects compiles on the JS side any more. The one divergence is
`scope_slot_reuse`, twin-consistent and quarantined with its reason. In order of payoff:

1. **The remaining refusals.** `instr` for Fldrq/AtomicThreadFence/CpuRelax/Bswap/Bsf/TraceTable/
   VolatileStore, the two `{.assembler.}` fixtures, and the `futex`/`mmap`/`ulock` syscalls. A few
   of the `instr` map onto plain JS/BigInt; the rest should stay refusals and be *listed* as such
   rather than discovered by whoever hits them. (The four `mod_*` lines in the report are not
   refusals of a construct: they are foreign-module fixtures with no `exportc "main"` to enter.)
2. **M9 — verification + docs.** `jorogumoTests` in `nativenif/tests/tester.nim` with the refusal
   list explicit, so `tjsgen`'s number stops being the only thing standing between a regression
   and a release; `hastur jsdiff` over `tests/jorogumo/` with the native back end as the oracle,
   the way `hastur wasmdiff` does for wasm; promote this plan to `nativenif/doc/jorogumo.md`; and
   bump `src/nativenif.commit`, which is also what lets a JS suite into nimony's CI at all.

## Workflow notes

- Branch `js-backend` in **both** repos; nativenif lands first (the tool), then the nimony PR
  carries driver + tests + the `src/nativenif.commit` pin bump.
- `syncNativenif` (`src/hastur/deps.nim:88`) deliberately leaves a clean checkout *on a branch*
  alone (warning only) — developing on a nativenif branch is safe; do not expect hastur to build
  the pin while the branch is checked out.
