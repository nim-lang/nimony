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
for (const m of iface.members) {
  if (m.type === "constructor") {
    if (m.arguments.some((a) => a.variadic)) { skipped.push(`constructor (variadic)`); continue; }
    const req = m.arguments.filter((a) => !a.optional); // required args only (v1)
    const params = req.map((a) => `${nid(a.name)}: ${mapType(a.idlType).nim}`).join("; ");
    const jsArgs = req.map((a) => mapType(a.idlType).toJs(nid(a.name)));
    const body = jsArgs.length === 0
      ? `newOf("${ifaceName}")`
      : `newOf("${ifaceName}", [${jsArgs.join(", ")}])`;
    out.push(`proc new${ifaceName}*(${params}): ${ifaceName} =`);
    out.push(`  ${body}`);
    if (m.arguments.some((a) => a.optional)) skipped.push(`constructor optional arg(s): ${m.arguments.filter(a=>a.optional).map(a=>a.name).join(", ")}`);
    out.push(``);
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
    if (m.arguments.some((a) => a.variadic || a.optional)) { skipped.push(`op ${m.name} (optional/variadic args)`); continue; }
    if (m.arguments.length > 3) { skipped.push(`op ${m.name} (>3 args — needs apply)`); continue; }
    const params = m.arguments.map((a) => `${nid(a.name)}: ${mapType(a.idlType).nim}`).join("; ");
    const sig = params ? `${self}: ${ifaceName}; ${params}` : `${self}: ${ifaceName}`;
    const callArgs = m.arguments.map((a) => mapType(a.idlType).toJs(nid(a.name)));
    const callExpr = callArgs.length
      ? `${self}.call("${m.name}", ${callArgs.join(", ")})`
      : `${self}.call("${m.name}")`;
    const ret = m.idlType ? m.idlType.idlType : "undefined";
    if (ret === "undefined") {
      out.push(`proc ${nid(m.name)}*(${sig}) = discard ${callExpr}`);
    } else {
      const t = mapType(m.idlType);
      out.push(`proc ${nid(m.name)}*(${sig}): ${t.nim} = ${t.fromJs(callExpr)}`);
    }
    out.push(``);
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
