// DOM environment for tclasslist (element.classList is a DOMTokenList, which
// only exists inside a DOM). Same jsdom setup as tdom.env.js.
const { JSDOM } = require("jsdom");
const _dom = new JSDOM("<!doctype html><html><head></head><body></body></html>");
globalThis.window = _dom.window;
globalThis.document = _dom.window.document;
globalThis.Event = _dom.window.Event;
globalThis.HTMLElement = _dom.window.HTMLElement;
