## Regression: a bare parameterless template used as a *value* (here, a call
## argument) must be EXPANDED — `MAGIC_HANDLE` means `MAGIC_HANDLE()`.
##
## Previously `semExprSym` treated a template symbol used bare as a
## first-class routine value and emitted the template symbol into the typed
## tree. Templates have no runtime value and lengc has no decl to mangle for
## one, so codegen aborted with
##   `Symbol not found in NIF module: MAGIC_HANDLE.0.<mod>`.
## Fix: expand the bare template as an implicit zero-arg call (except in a
## callee position, where `semCall` drives the expansion itself).
##
## Pattern: a `template HKEY_LOCAL_MACHINE*: HKEY = cast[HKEY](...)`-style
## parameterless template passed cross-module as an argument to an importc'd proc.

import std / syncio
import deps / mtemplatevalue

var hits = 0

# bare template as a call argument (the crash case):
if probe(MAGIC_HANDLE) != 0'i32:
  hits = 1

# bare template as a var initializer (also a value context):
let h = MAGIC_HANDLE
if probe(h) != 0'i32:
  hits = hits + 1

echo "hits=", hits
