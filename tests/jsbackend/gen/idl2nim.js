#!/usr/bin/env node
// WebIDL -> Nim binding generator for the JS backend.
//
//   node gen/idl2nim.js <spec> <Interface> <out.nim>
//     e.g.  node gen/idl2nim.js url URL weburl.nim
//
// Reads the curated spec IDL shipped in @webref/idl (the W3C/WHATWG official
// data), parses it with the standard `webidl2` parser, and emits a jsffi-based
// binding in the exact style of the hand-written dom.nim — proving the path to
// spec-backed bindings. This is a deliberately small first cut: it handles
// attributes (get/set), the constructor, and fixed-arity instance operations.
// Constructs it does not yet emit (optional/variadic args, static ops,
// iterables, overloads) are reported as `## SKIPPED` comments so the coverage
// gap is visible in the output rather than silent.
const fs = require("fs");
const path = require("path");
const webidl2 = require("webidl2");

const [, , spec, ifaceName, outFile] = process.argv;
if (!spec || !ifaceName || !outFile) {
  console.error("usage: node gen/idl2nim.js <spec> <Interface> <out.nim>");
  process.exit(2);
}

const idlPath = path.join(__dirname, "..", "node_modules", "@webref", "idl", spec + ".idl");
const ast = webidl2.parse(fs.readFileSync(idlPath, "utf8"));
const iface = ast.find((x) => x.type === "interface" && x.name === ifaceName);
if (!iface) {
  console.error(`interface ${ifaceName} not found in ${spec}.idl`);
  process.exit(1);
}

// WebIDL type -> how it marshals through jsffi. `nim` is the Nim type; `toJs`
// wraps a Nim value for a call/attr-set; `fromJs` unwraps a JsValue result.
// Anything unrecognised (other interfaces, `any`, sequences, …) stays a JsValue.
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

const out = [];
const skipped = [];
out.push(`## GENERATED from @webref/idl (${spec}.idl) by gen/idl2nim.js — do not edit by hand.`);
out.push(`## Regenerate: node gen/idl2nim.js ${spec} ${ifaceName} ${outFile}`);
out.push(`##`);
out.push(`## A jsffi binding for the WHATWG/W3C \`${ifaceName}\` interface. Each member`);
out.push(`## marshals through jsffi exactly as the hand-written dom.nim does.`);
out.push(`import jsffi`);
out.push(``);
out.push(`type`);
out.push(`  ${ifaceName}* = JsValue`);
if (iface.inheritance) {
  out.push(`    ## NOTE: inherits ${iface.inheritance} (flat JsValue; inherited members not re-emitted).`);
}
out.push(``);

const self = "self";
const paramOf = (a) => `${nid(a.name)}: ${mapType(a.idlType).nim}`;

for (const m of iface.members) {
  if (m.type === "constructor") {
    const args = m.arguments;
    if (args.length && args[args.length - 1].variadic) { skipped.push(`constructor (variadic)`); continue; }
    // One overload per arity, from the required count up to all args (WebIDL
    // guarantees optionals trail), so `new URL(url)` and `new URL(url, base)`
    // both exist.
    const required = args.filter((a) => !a.optional).length;
    for (let k = required; k <= args.length; k++) {
      const sub = args.slice(0, k);
      const params = sub.map(paramOf).join("; ");
      const jsArgs = sub.map((a) => mapType(a.idlType).toJs(nid(a.name)));
      const body = jsArgs.length === 0
        ? `newOf("${ifaceName}")`
        : `newOf("${ifaceName}", [${jsArgs.join(", ")}])`;
      out.push(`proc new${ifaceName}*(${params}): ${ifaceName} =`);
      out.push(`  ${body}`);
      out.push(``);
    }
  } else if (m.type === "attribute") {
    const t = mapType(m.idlType);
    out.push(`proc ${nid(m.name)}*(${self}: ${ifaceName}): ${t.nim} = ${t.fromJs(`${self}.get("${m.name}")`)}`);
    if (!m.readonly) {
      out.push(`proc \`${m.name}=\`*(${self}: ${ifaceName}; value: ${t.nim}) = ${self}.set("${m.name}", ${t.toJs("value")})`);
    }
    out.push(``);
  } else if (m.type === "operation") {
    if (m.special === "static") { skipped.push(`static op ${m.name}`); continue; }
    if (!m.name) { skipped.push(`anonymous/special op (${m.special})`); continue; }
    const args = m.arguments;
    const ret = m.idlType ? m.idlType.idlType : "undefined";
    const rt = m.idlType ? mapType(m.idlType) : null;
    const retDecl = ret === "undefined" ? "" : `: ${rt.nim}`;

    if (args.length && args[args.length - 1].variadic) {
      // Variadic tail -> an openArray param, marshalled into a JS array and
      // spread via applyArgs (e.g. `classList.add("a", "b")`).
      const fixed = args.slice(0, -1);
      const va = args[args.length - 1];
      const params = [`${self}: ${ifaceName}`]
        .concat(fixed.map(paramOf))
        .concat([`${nid(va.name)}: openArray[${mapType(va.idlType).nim}]`])
        .join("; ");
      out.push(`proc ${nid(m.name)}*(${params})${retDecl} =`);
      out.push(`  let a = newJsArray()`);
      for (const a of fixed) out.push(`  a.add(${mapType(a.idlType).toJs(nid(a.name))})`);
      out.push(`  for x in ${nid(va.name)}: a.add(${mapType(va.idlType).toJs("x")})`);
      const call = `${self}.applyArgs("${m.name}", a)`;
      out.push(ret === "undefined" ? `  discard ${call}` : `  result = ${rt.fromJs(call)}`);
      out.push(``);
    } else {
      // Fixed/optional args -> one overload per arity (required..all).
      const required = args.filter((a) => !a.optional).length;
      for (let k = required; k <= args.length; k++) {
        if (k > 3) { skipped.push(`op ${m.name} arity ${k} (>3 fixed args)`); continue; }
        const sub = args.slice(0, k);
        const params = sub.map(paramOf).join("; ");
        const sig = params ? `${self}: ${ifaceName}; ${params}` : `${self}: ${ifaceName}`;
        const callArgs = sub.map((a) => mapType(a.idlType).toJs(nid(a.name)));
        const callExpr = callArgs.length
          ? `${self}.call("${m.name}", ${callArgs.join(", ")})`
          : `${self}.call("${m.name}")`;
        out.push(ret === "undefined"
          ? `proc ${nid(m.name)}*(${sig}) = discard ${callExpr}`
          : `proc ${nid(m.name)}*(${sig})${retDecl} = ${rt.fromJs(callExpr)}`);
      }
      out.push(``);
    }
  } else {
    skipped.push(`${m.type} member`);
  }
}

if (skipped.length) {
  out.push(`## SKIPPED (not yet generated — extend gen/idl2nim.js to cover):`);
  for (const s of skipped) out.push(`##   - ${s}`);
  out.push(``);
}

fs.writeFileSync(path.join(__dirname, "..", outFile), out.join("\n"));
console.error(`wrote ${outFile}: ${iface.members.length} members, ${skipped.length} skipped`);
