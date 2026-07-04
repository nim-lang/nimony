// DOM environment for tdomlib (typed DOM-tree navigation needs a live DOM).
// Same jsdom setup as tdom.env.js / tclasslist.env.js.
const { JSDOM } = require("jsdom");
const _dom = new JSDOM("<!doctype html><html><head></head><body></body></html>");
globalThis.window = _dom.window;
globalThis.document = _dom.window.document;
globalThis.Event = _dom.window.Event;
globalThis.HTMLElement = _dom.window.HTMLElement;
