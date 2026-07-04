#!/usr/bin/env node
// WebIDL -> Nim binding generator for the JS backend.
//
//   node gen/idl2nim.js <spec> <Interface> <out.nim> [MoreInterface...]
//     single:  node gen/idl2nim.js url URL weburl.nim
//     module:  node gen/idl2nim.js dom Node domlib.nim Element Document DocumentFragment
//
// Reads the curated spec IDL shipped in @webref/idl (the W3C/WHATWG official
// data), parses it with the standard `webidl2` parser, and emits a jsffi-based
// binding in the style of the hand-written dom.nim — proving the path to
// spec-backed bindings. It handles attributes (get/set), constructors, and
// operations, expanding optional args into per-arity overloads and a variadic
// tail into an openArray. It flattens the WebIDL `inheritance` chain and merges
// `includes` mixins, so a generated `Element` is self-contained. Given several
// interfaces it emits ONE module in which interface-typed members are typed by
// name (e.g. `getElementById` returns `Element`), enabling typed DOM-tree
// navigation. Constructs it does not yet emit (static ops, >3-arg fixed ops,
// iterables, consts) are reported as `## SKIPPED` comments so the coverage gap
// is visible rather than silent.
const fs = require("fs");
const path = require("path");
const webidl2 = require("webidl2");

const [, , spec, primaryIface, outFile, ...moreIfaces] = process.argv;
if (!spec || !primaryIface || !outFile) {
  console.error("usage: node gen/idl2nim.js <spec> <Interface> <out.nim> [MoreInterface...]");
  process.exit(2);
}
const genList = [primaryIface, ...moreIfaces];

const idlPath = path.join(__dirname, "..", "node_modules", "@webref", "idl", spec + ".idl");
const ast = webidl2.parse(fs.readFileSync(idlPath, "utf8"));

// --- registries -------------------------------------------------------------
const mixinByName = new Map();
for (const x of ast) if (x.type === "interface mixin") mixinByName.set(x.name, x);
const includedBy = new Map(); // interface name -> [mixin name, …] in file order
for (const x of ast) if (x.type === "includes") {
  if (!includedBy.has(x.target)) includedBy.set(x.target, []);
  includedBy.get(x.target).push(x.includes);
}
const ifaceByName = new Map();
for (const x of ast) if (x.type === "interface") ifaceByName.set(x.name, x);
// Every interface/mixin/dictionary/enum name — anything WebIDL treats as a
// named type. A member typed by one of these is a JS object handle, so it maps
// to that name (a JsValue alias) rather than raw JsValue.
const namedTypes = new Set();
for (const x of ast) if (["interface", "interface mixin", "dictionary", "enum", "callback", "callback interface"].includes(x.type)) namedTypes.add(x.name);

for (const g of genList) if (!ifaceByName.has(g)) {
  console.error(`interface ${g} not found in ${spec}.idl`);
  process.exit(1);
}

// Interface names referenced by an emitted member's type — declared as aliases
// so the generated module is self-typed. genList members go in unconditionally.
const referenced = new Set(genList);

// WebIDL type -> how it marshals through jsffi. `nim` is the Nim type; `toJs`
// wraps a Nim value for a call/attr-set; `fromJs` unwraps a JsValue result.
function mapType(t) {
  const name = typeof t === "string" ? t : t.idlType;
  switch (name) {
    case "DOMString": case "USVString": case "ByteString": case "CSSOMString":
      return { nim: "string", toJs: (e) => `toJs(${e})`, fromJs: (e) => `$${e}` };
    case "boolean":
      return { nim: "bool", toJs: (e) => `toJs(${e})`, fromJs: (e) => `${e}.toBool` };
    case "byte": case "octet": case "short": case "unsigned short":
    case "long": case "unsigned long": case "long long": case "unsigned long long":
      return { nim: "int", toJs: (e) => `toJs(${e})`, fromJs: (e) => `${e}.toInt` };
    case "float": case "unrestricted float": case "double": case "unrestricted double":
      return { nim: "float", toJs: (e) => `toJs(${e})`, fromJs: (e) => `${e}.toFloat` };
    default:
      // A named interface/dictionary type -> a JsValue alias by that name, so
      // the surface reads like a real DOM API and is distinct-ready.
      if (typeof name === "string" && namedTypes.has(name)) {
        referenced.add(name);
        return { nim: name, toJs: (e) => e, fromJs: (e) => e };
      }
      return { nim: "JsValue", toJs: (e) => e, fromJs: (e) => e };
  }
}

// Nim keywords that would collide with an emitted identifier -> backtick-quote.
const NIM_KEYWORDS = new Set([
  "addr","and","as","asm","bind","block","break","case","cast","concept","const",
  "continue","converter","defer","discard","distinct","div","do","elif","else","end",
  "enum","except","export","finally","for","from","func","if","import","in","include",
  "interface","is","isnot","iterator","let","macro","method","mixin","mod","nil","not",
  "notin","object","of","or","out","proc","ptr","raise","ref","return","shl","shr",
  "static","template","try","tuple","type","using","var","when","while","xor","yield",
]);
const nid = (n) => (NIM_KEYWORDS.has(n) ? "`" + n + "`" : n);
// jsffi's own accessor verbs take `(JsValue, string)` and differ only by return
// type, so a same-named DOM member (e.g. URLSearchParams.get(name)) would be a
// return-type-only redefinition Nim can't resolve. Skip just those. Verbs whose
// jsffi form has a distinct arg shape (add/len over arrays) are NOT listed —
// e.g. DOMTokenList.add(openArray[string]) is a fine, non-colliding overload.
const JSFFI_VERBS = new Set(["get", "set", "call", "apply", "applyArgs"]);

// --- inheritance + mixin flattening -----------------------------------------
function directMembers(name) {
  const host = ifaceByName.get(name);
  const ms = host ? host.members.slice() : [];
  for (const mx of includedBy.get(name) || []) {
    const mixin = mixinByName.get(mx);
    // NB: push members as-is — webidl2 exposes `type`/`name` via prototype
    // getters, so an object spread ({...m}) would silently drop them.
    if (mixin) for (const m of mixin.members) ms.push(m);
  }
  return ms;
}

// self -> ancestors, collecting direct members, most-derived shadowing.
function flatten(ifaceName) {
  const members = [];
  const seen = new Set();
  const mergedMixins = new Set();
  const flattenedFrom = [];
  const unresolved = [];
  let cur = ifaceName, level = 0;
  while (cur) {
    const host = ifaceByName.get(cur);
    if (!host) { unresolved.push(cur); break; }
    if (level > 0) flattenedFrom.push(cur);
    for (const mx of includedBy.get(cur) || []) if (mixinByName.has(mx)) mergedMixins.add(mx);
    for (const m of directMembers(cur)) {
      if (m.type === "constructor") { if (level === 0) members.push(m); continue; }
      const key = m.type === "operation" ? `op:${m.name}:${m.arguments.length}` : `${m.type}:${m.name}`;
      if (seen.has(key)) continue;
      seen.add(key);
      members.push(m);
    }
    cur = host.inheritance;
    level++;
  }
  return { members, mergedMixins, flattenedFrom, unresolved };
}

// --- emission ---------------------------------------------------------------
// All interfaces in a module share one JsValue overload space, so the SAME
// flattened member (appendChild on Element AND Node) collapses to one Nim
// signature. `emitted` dedups by name + arity-shape so we emit it once.
const emitted = new Set();
const paramOf = (a) => `${nid(a.name)}: ${mapType(a.idlType).nim}`;
const skipped = [];

function emitInterface(ifaceName, body) {
  const { members, mergedMixins, flattenedFrom, unresolved } = flatten(ifaceName);
  body.push(`# ── ${ifaceName} ─────────────────────────────────────────────────────────────`);
  if (flattenedFrom.length) body.push(`#   inherits: ${flattenedFrom.join(", ")}`);
  if (mergedMixins.size) body.push(`#   mixins:   ${[...mergedMixins].join(", ")}`);
  for (const u of unresolved) body.push(`#   NOTE: ancestor ${u} not in ${spec}.idl — its members are absent.`);
  body.push(``);
  const self = "self";

  for (const m of members) {
    if (m.type === "constructor") {
      const args = m.arguments;
      if (args.length && args[args.length - 1].variadic) { skipped.push(`${ifaceName} constructor (variadic)`); continue; }
      const required = args.filter((a) => !a.optional).length;
      for (let k = required; k <= args.length; k++) {
        const sub = args.slice(0, k);
        const params = sub.map(paramOf).join("; ");
        const jsArgs = sub.map((a) => mapType(a.idlType).toJs(nid(a.name)));
        const sig = `new${ifaceName}(${params})`;
        if (emitted.has(sig)) continue; emitted.add(sig);
        const call = jsArgs.length === 0 ? `newOf("${ifaceName}")` : `newOf("${ifaceName}", [${jsArgs.join(", ")}])`;
        body.push(`proc new${ifaceName}*(${params}): ${ifaceName} =`);
        body.push(`  ${call}`);
        body.push(``);
      }
    } else if (m.type === "attribute") {
      if (JSFFI_VERBS.has(m.name)) { skipped.push(`${ifaceName}.${m.name} attr (clashes with jsffi verb)`); continue; }
      const t = mapType(m.idlType);
      const getSig = `${m.name}/get`;
      if (!emitted.has(getSig)) {
        emitted.add(getSig);
        body.push(`proc ${nid(m.name)}*(${self}: ${ifaceName}): ${t.nim} = ${t.fromJs(`${self}.get("${m.name}")`)}`);
      }
      if (!m.readonly) {
        const setSig = `${m.name}/set:${t.nim}`;
        if (!emitted.has(setSig)) {
          emitted.add(setSig);
          body.push(`proc \`${m.name}=\`*(${self}: ${ifaceName}; value: ${t.nim}) = ${self}.set("${m.name}", ${t.toJs("value")})`);
        }
      }
      body.push(``);
    } else if (m.type === "operation") {
      if (m.special === "static") { skipped.push(`${ifaceName}.${m.name} static op`); continue; }
      if (!m.name) { skipped.push(`${ifaceName} anonymous/special op (${m.special})`); continue; }
      if (JSFFI_VERBS.has(m.name)) { skipped.push(`${ifaceName}.${m.name} op (clashes with jsffi verb)`); continue; }
      const args = m.arguments;
      const ret = m.idlType ? m.idlType.idlType : "undefined";
      const rt = m.idlType ? mapType(m.idlType) : null;
      const retDecl = ret === "undefined" ? "" : `: ${rt.nim}`;

      if (args.length && args[args.length - 1].variadic) {
        const fixed = args.slice(0, -1);
        const va = args[args.length - 1];
        const sig = `${m.name}/va:${fixed.length}`;
        if (emitted.has(sig)) continue; emitted.add(sig);
        const params = [`${self}: ${ifaceName}`]
          .concat(fixed.map(paramOf))
          .concat([`${nid(va.name)}: openArray[${mapType(va.idlType).nim}]`]).join("; ");
        body.push(`proc ${nid(m.name)}*(${params})${retDecl} =`);
        body.push(`  let a = newJsArray()`);
        for (const a of fixed) body.push(`  a.add(${mapType(a.idlType).toJs(nid(a.name))})`);
        body.push(`  for x in ${nid(va.name)}: a.add(${mapType(va.idlType).toJs("x")})`);
        const call = `${self}.applyArgs("${m.name}", a)`;
        body.push(ret === "undefined" ? `  discard ${call}` : `  result = ${rt.fromJs(call)}`);
        body.push(``);
      } else {
        const required = args.filter((a) => !a.optional).length;
        for (let k = required; k <= args.length; k++) {
          if (k > 3) { skipped.push(`${ifaceName}.${m.name} arity ${k} (>3 fixed args)`); continue; }
          const sig = `${m.name}/op:${k}`;
          if (emitted.has(sig)) continue; emitted.add(sig);
          const sub = args.slice(0, k);
          const params = sub.map(paramOf).join("; ");
          const sigParams = params ? `${self}: ${ifaceName}; ${params}` : `${self}: ${ifaceName}`;
          const callArgs = sub.map((a) => mapType(a.idlType).toJs(nid(a.name)));
          const callExpr = callArgs.length ? `${self}.call("${m.name}", ${callArgs.join(", ")})` : `${self}.call("${m.name}")`;
          body.push(ret === "undefined"
            ? `proc ${nid(m.name)}*(${sigParams}) = discard ${callExpr}`
            : `proc ${nid(m.name)}*(${sigParams})${retDecl} = ${rt.fromJs(callExpr)}`);
        }
        body.push(``);
      }
    } else {
      skipped.push(`${ifaceName} ${m.type} member`);
    }
  }
}

// mapType (via emitInterface) fills `referenced`, so emit bodies first, then
// assemble the header + type block once all referenced aliases are known.
const body = [];
for (const g of genList) emitInterface(g, body);

const header = [];
header.push(`## GENERATED from @webref/idl (${spec}.idl) by gen/idl2nim.js — do not edit by hand.`);
header.push(`## Regenerate: node gen/idl2nim.js ${spec} ${primaryIface} ${outFile}${moreIfaces.length ? " " + moreIfaces.join(" ") : ""}`);
header.push(`##`);
header.push(`## jsffi bindings for the WHATWG/W3C ${genList.join(", ")} interface${genList.length > 1 ? "s" : ""}.`);
header.push(`## Each interface is a JsValue alias; interface-typed members are typed by name`);
header.push(`## so DOM-tree navigation reads (and, once distinct, checks) like a real DOM API.`);
header.push(`import jsffi`);
header.push(``);
header.push(`type`);
// genList first (the real subjects), then the extra interfaces they reference.
const aliasNames = [...genList, ...[...referenced].filter((n) => !genList.includes(n)).sort()];
for (const n of aliasNames) header.push(`  ${n}* = JsValue`);
header.push(``);

const out = header.concat(body);
if (skipped.length) {
  out.push(`## SKIPPED (not yet generated — extend gen/idl2nim.js to cover):`);
  for (const s of skipped) out.push(`##   - ${s}`);
  out.push(``);
}

fs.writeFileSync(path.join(__dirname, "..", outFile), out.join("\n"));
console.error(`wrote ${outFile}: ${genList.length} interface(s), ${aliasNames.length} aliases, ${emitted.size} procs, ${skipped.length} skipped`);
