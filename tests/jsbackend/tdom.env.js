// DOM environment for the JS-backend DOM test: a real WHATWG DOM via jsdom,
// exposed as the globals the compiled Nim reads (`document`, `window`, `Event`).
// The runner prepends this to the bundle (before runtime.js) and runs node with
// tests/jsbackend/node_modules on NODE_PATH. jsdom implements the actual DOM
// spec, so the bindings in dom.nim are validated against real DOM semantics —
// the same environment a WebIDL-generated binding layer will target.
const { JSDOM } = require("jsdom");
const _dom = new JSDOM("<!doctype html><html><head></head><body></body></html>");
globalThis.window = _dom.window;
globalThis.document = _dom.window.document;
// Use jsdom's Event/EventTarget so `new Event(...)` and a node's `.target`
// share jsdom's realm (Node's native Event would be a different constructor).
globalThis.Event = _dom.window.Event;
globalThis.HTMLElement = _dom.window.HTMLElement;
