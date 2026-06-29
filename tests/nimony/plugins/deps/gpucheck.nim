## `.gpu` restriction checker — a FRONTEND MODULE PLUGIN (triggered by
## `{.plugin: "deps/gpucheck".}`). It receives the whole semmed module, enforces
## the GPU coloring discipline, and passes the module through, replacing any
## offending construct with a compiler error (`(err …)`). This is the
## "color/checker" half of the backend-plugin protocol; `ghast` (the SPIR-V
## emitter) is the other.
##
## Rules enforced (v1, intra-module):
##  * a `{.gpu.}` proc may only call other `{.gpu.}` procs of THIS module —
##    calling a same-module non-`.gpu` proc is rejected. (Calls to magics/other
##    modules — `+`, `echo`, … — are not module procs, so they are left to a
##    later cross-module / intrinsic-whitelist pass; not flagged here.)
##  * a `{.gpu.}` proc may not `raise` (exceptions are unavailable on the device).
##
## Transitivity is automatic: each `.gpu` proc asserts its DIRECT callees are
## `.gpu`, so the whole reachable subgraph is `.gpu` — no whole-program pass,
## IC-friendly. The `{.gpu.}` color is detected as a preserved `(pragma <sym>)`
## whose symbol's display name is `gpu`.

import plugins

proc has(s: seq[SymId]; x: SymId): bool =
  for e in s:
    if e == x: return true
  result = false

proc displayName(s: SymId): string =
  ## The readable prefix of a mangled symbol (`kernel.0.mymod` -> `kernel`).
  result = ""
  for ch in symText(s):
    if ch == '.': break
    result.add ch

proc isGpuProc(procN: NifCursor): bool =
  ## True if the proc's pragmas carry `{.gpu.}`, preserved as a `(pragma <sym>)`
  ## whose symbol display-name is `gpu`.
  result = false
  var c = procN
  c.into:
    while c.kind != ParRi:
      if c.kind == ParLe and c.otherKind == PragmasU:
        var p = c
        p.into:
          while p.kind != ParRi:
            if p.kind == ParLe:
              let sym = firstChild(p)
              if sym.kind == Symbol and displayName(sym.symId) == "gpu":
                result = true
            skip p
      skip c

proc procName(procN: NifCursor): SymId =
  let nm = firstChild(procN)
  if nm.kind == SymbolDef: result = nm.symId
  else: result = default(SymId)

proc collectProcs(root: NifCursor; allProcs, gpuSet: var seq[SymId]) =
  ## Every top-level proc symbol, and the `.gpu`-colored subset.
  var c = root
  c.into:
    while c.kind != ParRi:
      if c.kind == ParLe and c.stmtKind == ProcS:
        let nm = procName(c)
        if nm != default(SymId):
          allProcs.add nm
          if isGpuProc(c): gpuSet.add nm
      skip c

proc tr(t: var Replacer; inGpu: bool; caller: string;
        allProcs, gpuSet: seq[SymId]) =
  ## Copy the module through; inside a `.gpu` proc, replace a forbidden `raise`
  ## or a call to a same-module non-`.gpu` proc with an `(err …)`.
  if t.isAtom:
    keep t, Any
  elif t.stmtKind == ProcS:
    var isg = false
    var nm = ""
    peek t:
      let p = getCursor(t)
      isg = isGpuProc(p)
      nm = displayName(procName(p))
    loopKeepTag t:
      tr(t, isg, nm, allProcs, gpuSet)
  elif inGpu and t.stmtKind == RaiseS:
    t.replace(RaiseS, errorTree("gpu proc '" & caller &
      "' may not raise; exceptions are unavailable on the device", t.info))
  elif inGpu and t.exprKind == CallX:
    var bad = false
    var callee = ""
    peek t:
      let c = firstChild(getCursor(t))
      if c.kind == Symbol and has(allProcs, c.symId) and not has(gpuSet, c.symId):
        bad = true
        callee = displayName(c.symId)
    if bad:
      t.replace(CallX, errorTree("gpu proc '" & caller &
        "' may only call gpu procs, but calls '" & callee & "'", t.info))
    else:
      loopKeepTag t:
        tr(t, inGpu, caller, allProcs, gpuSet)
  else:
    loopKeepTag t:
      tr(t, inGpu, caller, allProcs, gpuSet)

var t = loadReplacer()
var allProcs: seq[SymId] = @[]
var gpuSet: seq[SymId] = @[]
peek t:
  collectProcs(getCursor(t), allProcs, gpuSet)
loopKeepTag t:
  tr(t, false, "", allProcs, gpuSet)
saveReplacer(t)
