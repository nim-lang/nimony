## `.gpu` restriction checker — a frontend module plugin triggered by
## `{.plugin: "deps/gpucheck".}`. It receives the whole semmed module, enforces
## the GPU coloring discipline, and passes the module through, replacing any
## offending construct with a compiler error (`(err …)`).
##
## Rules enforced (v1, intra-module):
## * a `{.gpu.}` proc may only call other `{.gpu.}` procs from this module;
## * a `{.gpu.}` proc may not raise.

import plugins

proc has(s: seq[SymId]; x: SymId): bool =
  for e in s:
    if e == x:
      return true
  result = false

proc displayName(s: SymId): string =
  ## Returns the readable prefix of a mangled symbol.
  result = ""
  for ch in symText(s):
    if ch == '.':
      break
    result.add ch

proc isGpuProc(procNode: NifCursor): bool =
  ## Checks for a preserved `(pragma <sym>)` whose symbol is named `gpu`.
  var scan = procNode
  scan.linearScan:
    if scan.pragmaKind == PragmaP:
      let sym = firstChild(scan)
      if sym.kind == Symbol and displayName(sym.symId) == "gpu":
        return true
  result = false

proc procName(procNode: NifCursor): SymId =
  let name = firstChild(procNode)
  if name.kind == SymbolDef:
    result = name.symId
  else:
    result = default(SymId)

proc collectProcs(root: NifCursor; allProcs, gpuProcs: var seq[SymId]) =
  ## Collects all top-level proc symbols and the `.gpu`-colored subset.
  var child = root
  child.loopInto:
    if child.kind == TagLit and child.stmtKind == ProcS:
      let name = procName(child)
      if name != default(SymId):
        allProcs.add name
        if isGpuProc(child):
          gpuProcs.add name
    child.skip()

proc transform(r: var Replacer; inGpu: bool; caller: string;
               allProcs, gpuProcs: seq[SymId]) =
  ## Passes the module through, replacing forbidden constructs with errors.
  if r.isAtom:
    keep r, Any
  elif r.stmtKind == ProcS:
    var isGpu = false
    var name = ""
    peek r:
      let procNode = getCursor(r)
      isGpu = isGpuProc(procNode)
      name = displayName(procName(procNode))
    loopKeepTag r:
      transform(r, isGpu, name, allProcs, gpuProcs)
  elif inGpu and r.stmtKind == RaiseS:
    replace r, RaiseS, errorTree("gpu proc '" & caller &
      "' may not raise; exceptions are unavailable on the device", r.info)
  elif inGpu and r.exprKind == CallX:
    var bad = false
    var callee = ""
    peek r:
      let target = firstChild(getCursor(r))
      if target.kind == Symbol and
          has(allProcs, target.symId) and not has(gpuProcs, target.symId):
        bad = true
        callee = displayName(target.symId)
    if bad:
      replace r, CallX, errorTree("gpu proc '" & caller &
        "' may only call gpu procs, but calls '" & callee & "'", r.info)
    else:
      loopKeepTag r:
        transform(r, inGpu, caller, allProcs, gpuProcs)
  else:
    loopKeepTag r:
      transform(r, inGpu, caller, allProcs, gpuProcs)

var r = loadReplacer()
var allProcs: seq[SymId] = @[]
var gpuProcs: seq[SymId] = @[]
peek r:
  collectProcs(getCursor(r), allProcs, gpuProcs)
loopKeepTag r:
  transform(r, false, "", allProcs, gpuProcs)
saveReplacer(r)
