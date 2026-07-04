## Exercises `domlib.nim` — Node / Element / Document / DocumentFragment
## generated as ONE module by gen/idl2nim.js, with interface-typed members. So
## DOM-tree navigation is typed end to end: createElement / getElementById /
## querySelector / firstElementChild return `Element`, ownerDocument returns
## `Document`, appendChild takes and returns `Node` — all from one import.
## Needs a live DOM (tdomlib.env.js, jsdom).
import std/syncio
import jsffi
import domlib

let doc: Document = global("document")
let body: Node = doc.get("body")

# Build  <div id="app"><h1>Hello</h1><p>world</p></div>  from typed nodes.
let root: Element = doc.createElement("div")
root.id = "app"
let title: Element = doc.createElement("h1")
title.textContent = "Hello"
let para: Element = doc.createElement("p")
para.textContent = "world"
root.append([title, para])              # ParentNode variadic, typed Element args
discard body.appendChild(root)          # Node op, typed Node in/out

echo "childElementCount: ", root.childElementCount
let first: Element = root.firstElementChild                # -> Element
echo "first tag/text: ", first.tagName, " ", first.textContent
let found: Element = doc.getElementById("app")             # -> Element
echo "getElementById tag: ", found.tagName
echo "querySelector p: ", root.querySelector("p").textContent
let owner: Document = title.ownerDocument                  # -> Document
echo "owner->app tag: ", owner.getElementById("app").tagName
echo "documentElement tag: ", doc.documentElement.tagName
