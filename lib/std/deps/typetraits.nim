## Plugin backing `std/typetraits`. One binary serves every trait in that
## module; `pluginName` says which template was called.
##
## Input 1 (`loadPluginInput`):  `(stmts <traitName> <type>)`
## Input 2 (`loadTypeDefinitions`): the declarations this plugin has asked for,
##   `(stmts (type :Sym <export> <typevars> <pragmas> <body>)*)`, empty at first.
##
## Structural types (`ptr`, `array`, `tuple`, …) arrive expanded in input 1, but
## a nominal one arrives as a bare `Symbol` — and a plugin, running in its own
## process, cannot look it up. So it asks: `needTypes(sym)` makes the compiler
## append that declaration and run the plugin again. `distinctBase` of a chain
## therefore takes one round per `distinct` layer, each asking only for the
## symbol it just uncovered.

import plugins

const
  # Child slots of a `(type :Name <export> <typevars> <pragmas> <body>)` decl.
  NameSlot = 0
  TypevarsSlot = 2
  BodySlot = 4

proc slot(decl: NifCursor; idx: int): NifCursor =
  ## Cursor at child `idx` of a declaration.
  result = firstChild(decl)
  for _ in 0 ..< idx:
    skip result

proc findDecl(defs: NifCursor; s: SymId; found: var bool): NifCursor =
  ## Linear scan of the declarations provided so far. Only what this plugin
  ## asked for is ever in there, so the list stays short and a scan beats
  ## building a table.
  result = defs
  found = false
  var n = defs
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if not found:
          var name = slot(n, NameSlot)
          if name.symId == s:
            result = n
            found = true
        skip n

proc findTypeDecl(defs: NifCursor; s: SymId; found: var bool): NifCursor =
  ## As `findDecl`, but reports "not found" for anything that is not a
  ## `(type …)` section — a type variable in particular.
  result = findDecl(defs, s, found)
  if found and result.stmtKind != TypeS:
    found = false

proc isTypevar(defs: NifCursor; s: SymId): bool =
  ## The compiler answers a request for a type variable with its `(typevar …)`
  ## declaration, so this is a positive test. Inferring "typevar" from a symbol's
  ## absence would instead report every not-yet-requested type as one.
  var found = false
  let decl = findDecl(defs, s, found)
  result = found and decl.otherKind in {TypevarU, StaticTypevarU}

proc collectUnknown(defs: NifCursor; n: NifCursor; missing: var seq[SymId]) =
  ## Every symbol in `n` that has not been provided yet. Collected across the
  ## whole argument, so one request covers a level instead of one round each.
  var n = n
  if n.kind == Symbol:
    var found = false
    discard findDecl(defs, n.symId, found)
    if not found:
      missing.add n.symId
  elif n.kind == TagLit:
    n.into:
      while n.hasMore:
        collectUnknown(defs, n, missing)
        skip n

proc unwrap(n: NifCursor): NifCursor =
  ## `typeof(x)` reaches a plugin as `(typedesc X)` while a written-out type
  ## expression does not, so normalize before looking at the type itself.
  result = n
  if result.typeKind == TypedescT:
    result = firstChild(result)

proc distinctBase(defs, arg: NifCursor; recursive: bool;
                  missing: var seq[SymId]): NifCursor =
  ## Peels `distinct` layers. Non-distinct types pass through unchanged, which
  ## is what makes `distinctBase(int) is int` hold. Stops and records a request
  ## when it uncovers a symbol it has not been given yet.
  result = arg
  var fuel = 100
  while fuel > 0 and result.kind == Symbol:
    dec fuel
    var found = false
    let decl = findTypeDecl(defs, result.symId, found)
    if not found:
      missing.add result.symId
      break
    let body = slot(decl, BodySlot)
    if body.typeKind != DistinctT: break
    result = firstChild(body)
    if not recursive: break

proc genericHead(defs, arg: NifCursor; o: var NifBuilder): bool =
  ## A generic instance's declaration carries `(at <head> <args>…)` in its
  ## typevars slot — the uninstantiated symbol and the arguments it was
  ## instantiated with. That is exactly `genericHead` plus `genericParams`.
  ## Emits nothing and returns false when `arg` is not a generic instance.
  result = false
  if arg.kind != Symbol: return
  var declFound = false
  let decl = findTypeDecl(defs, arg.symId, declFound)
  if not declFound: return
  let typevars = slot(decl, TypevarsSlot)
  if typevars.typeKind != AtT: return
  o.addSubtree firstChild(typevars)
  result = true

proc tr(n: NifCursor; defs: NifCursor): NifBuilder =
  let trait = pluginName(n)
  var arg = unwrap(callArgs(n))

  # First round on a nominal argument: nothing has been provided yet, so ask.
  var missing: seq[SymId] = @[]
  collectUnknown(defs, arg, missing)
  if missing.len > 0:
    return needTypes(missing)

  if arg.kind == Symbol and isTypevar(defs, arg.symId):
    # Nothing here can be decided from a type variable: whether `T` is
    # `distinct`, or a generic instance, is a property of what it is bound to.
    # Ask again after instantiation.
    return deferExpansion()

  case trait
  of "distinctBase", "distinctBaseShallow":
    var wanted: seq[SymId] = @[]
    let base = distinctBase(defs, arg, trait == "distinctBase", wanted)
    if wanted.len > 0:
      # A deeper `distinct` layer named a symbol we have not been given yet.
      return needTypes(wanted)
    result = createTree()
    result.addSubtree base
  of "genericHead", "stripGenericParams":
    result = createTree()
    if not genericHead(defs, arg, result):
      if trait == "stripGenericParams":
        result.addSubtree arg
      else:
        result = errorTree("'genericHead' expects an instantiated generic type", n)
  else:
    result = errorTree("unknown type trait: '" & trait & "'", n)

var inp = loadPluginInput()
var defs = loadTypeDefinitions()
saveTree tr(inp, defs)
