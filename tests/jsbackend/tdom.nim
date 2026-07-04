## End-to-end DOM: build a tree from Nim data, query it back, and wire a Nim
## proc as a click handler — all against a real WHATWG DOM (jsdom, set up by
## tdom.env.js). This is the first program that exercises the `dom.nim` binding
## slice over `jsffi`.
import std/syncio
import jsffi
import dom

var clicks = 0
proc onClick(ev: JsValue) =
  inc clicks
  echo "clicked; target text: ", ev.target.textContent

let doc = document()

# Build <ul id="list"> with three <li> from Nim data, append to <body>.
let ul = doc.createElement("ul")
ul.id = "list"
let items = ["alpha", "beta", "gamma"]
for it in items:
  let li = doc.createElement("li")
  li.textContent = it
  discard ul.appendChild(li)
discard doc.body.appendChild(ul)

# Query the tree back.
let list = doc.getElementById("list")
echo "children: ", list.childCount
echo "first li: ", list.querySelector("li").textContent

# A <button> wired to a Nim click handler, appended to <body>.
let btn = doc.createElement("button")
btn.textContent = "Press"
btn.addEventListener("click", onClick)
discard doc.body.appendChild(btn)

echo "body html: ", doc.body.innerHTML

# Dispatch two clicks; the Nim handler fires each time and reads the target back.
discard btn.dispatchEvent(newEvent("click"))
discard btn.dispatchEvent(newEvent("click"))
echo "clicks: ", clicks
