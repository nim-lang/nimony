#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Transforms defer statements into try-finally blocks.

import std / [assertions]
include ".." / lib / nifprelude
import nimony_model, programs
include nif_annotations

type
  ActionItem = object
    id: int
      ## The enclosing scope, identified by the position its body starts at.
    pos: int
      ## Where this `defer`'s `try` BEGINS: the position the statement after it
      ## will be written to. A `defer` protects what follows it, not what
      ## precedes it, so this is not the scope start.
    action: TokenBuf
  Context = object
    scopeStack: seq[int]
    actionStack: seq[ActionItem]
    retSym: SymId

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)
  {.ensuresNif: addedAny(dest).}

proc takeUnexpectedChild(dest: var TokenBuf; n: var Cursor) =
  ## A child of an `if`/`case` that is not an `elif`/`of`/`else`. That is not
  ## hypothetical: `sem` appends its diagnostics as extra children of the very
  ## node they are about — `checkExhaustiveness` puts `(err "not all cases are
  ## covered")` after the last `of` — and this pass still runs over the erroneous
  ## tree. Skipping the rest with a `break` left those children unconsumed, so
  ## `copyInto`'s balance assertion fired and the compiler died on an assertion
  ## instead of printing the diagnostic. Copy it through and keep walking.
  dest.takeTree n

proc wrapOneDefer(dest: var TokenBuf; startsAt: int; action: var TokenBuf;
                  info: NifLineInfo) =
  ## Collect-then-wrap: nifcore's sealed model can't insert unbalanced opens,
  ## so instead of splicing `(try (stmts` in at `startsAt` we take the finished
  ## tail `dest[startsAt..]`, drop it, and re-emit it wrapped.
  ##
  ## `startsAt` is where the `defer` STOOD, not where its scope began: a
  ## `defer` protects the statements after it and nothing before it. Wrapping
  ## from the scope start instead pulled the scope's earlier declarations —
  ## `result` among them — inside the `try` body, which put them in a
  ## different scope from the `finally` that reads them and left every later
  ## pass to cope with a `try` body that was not a scope.
  var bodyBuf = createTokenBuf(dest.len - startsAt + 4)
  for i in startsAt ..< dest.len: bodyBuf.add dest[i]
  dest.shrink startsAt
  dest.addParLe(TryS, info)
  dest.addParLe(StmtsS, info)
  var bc = beginRead(bodyBuf)
  while bc.hasMore:
    dest.addSubtree bc
    skip bc
  dest.addParRi()                     # close the try's stmts
  dest.addParLe(FinU, info)
  var dc = beginRead(action)
  while dc.hasMore:
    dest.addSubtree dc
    skip dc
  dest.addParRi()                     # close (fin …)
  dest.addParRi()                     # close (try …)

proc wrapScopeDefers(c: var Context; dest: var TokenBuf; scopeId: int;
                     info: NifLineInfo) =
  ## Wrap every `defer` this scope collected. Popping the stack hands them
  ## back last-declared first, which is also innermost first: the last
  ## `defer`'s `try` starts latest and so must be built before the earlier
  ## ones, which then wrap it along with the statements between them.
  while c.actionStack.len > 0 and c.actionStack[^1].id == scopeId:
    var popped = c.actionStack.pop
    wrapOneDefer(dest, popped.pos, popped.action, info)

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let beforeBody = dest.len+1
  let blockInfo = n.info
  c.scopeStack.add beforeBody
  if n.stmtKind in {ScopeS, StmtsS}:
    dest.addParLe(n.cursorTagId, n.info)
    n.into:
      while n.hasMore:
        trStmt c, dest, n
  else:
    dest.addParLe(StmtsS, n.info)
    trStmt c, dest, n
  wrapScopeDefers(c, dest, beforeBody, blockInfo)
  dest.addParRi()
  discard c.scopeStack.pop

proc trDefer(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let mine = c.scopeStack[^1]
  # `dest.len` BEFORE anything else is emitted: the `defer` statement itself
  # writes nothing here (its body is captured into `deferBody`), so this is
  # exactly where the statements it protects begin.
  let startsAt = dest.len
  # capture the (transformed) defer body; the wrap happens at scope end
  var deferBody = createTokenBuf(50)
  n.into: # enter (defer
    trStmt c, deferBody, n
  c.actionStack.add ActionItem(id: mine, pos: startsAt, action: ensureMove deferBody)
proc trReturn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if c.retSym != NoSymId and not (n.childCursor.isSymbol and n.childCursor.symId == c.retSym):
    # transform to `result = <expr>; return result`, see bug #1440
    let info = n.info
    n.into: # consume the whole `(ret …)`; leaving its close unconsumed would
            # make every statement after the return silently dropped (the
            # caller's `while n.hasMore` would stop early)
      dest.copyIntoKind AsgnS, info:
        dest.addSymUse c.retSym, info
        trStmt c, dest, n
    dest.copyIntoKind RetS, info:
      dest.addSymUse c.retSym, info
  else:
    # ordinary recursion:
    copyInto dest, n:
      while n.hasMore:
        trStmt c, dest, n

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of Symbol, SymbolDef, UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, DotToken, Ident, StrLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeTree n
  of TagLit:
    case n.stmtKind
    of ProcS, FuncS, IteratorS, ConverterS, MethodS, TemplateS, MacroS, TypeS:
      dest.takeTree n
    of IfS:
      copyInto dest, n: # if
        while n.hasMore:
          let k = n.substructureKind
          if k == ElifU:
            copyInto dest, n: # elif
              trStmt c, dest, n
              trBlock c, dest, n
          elif k == ElseU:
            copyInto dest, n: # else
              trBlock c, dest, n
          else:
            takeUnexpectedChild dest, n
    of CaseS:
      copyInto dest, n: # case
        trStmt c, dest, n # subject
        while n.hasMore:
          let k = n.substructureKind
          if k == OfU:
            copyInto dest, n: # of
              trStmt c, dest, n
              trBlock c, dest, n
          elif k == ElifU:
            copyInto dest, n: # elif
              trStmt c, dest, n
              trBlock c, dest, n
          elif k == ElseU:
            copyInto dest, n: # else
              trBlock c, dest, n
          else:
            takeUnexpectedChild dest, n
    of ForS:
      copyInto dest, n: # for
        trStmt c, dest, n # iterator
        trStmt c, dest, n # variables
        trBlock c, dest, n
    of TryS:
      copyInto dest, n: # try
        trBlock c, dest, n
        while n.hasMore and n.substructureKind == ExceptU:
          copyInto dest, n: # except
            trStmt c, dest, n
            trBlock c, dest, n
        if n.hasMore and n.substructureKind == FinU:
          copyInto dest, n: # finally
            trBlock c, dest, n
    of WhileS, BlockS, CoroforS:
      copyInto dest, n: # while
        trStmt c, dest, n # condition or label
        trBlock c, dest, n
    of DeferS:
      trDefer c, dest, n
    of RetS:
      trReturn c, dest, n
    of ResultS:
      copyInto dest, n:
        assert n.isSymbolDef
        c.retSym = n.symId
        dest.takeTree n
        while n.hasMore:
          trStmt c, dest, n
    of CallS, CmdS, GvarS, TvarS, VarS, ConstS, GletS, TletS, LetS, CursorS,
       PatternvarS, EmitS, AsgnS, ScopeS, WhenS, BreakS, ContinueS, YldS,
       StmtsS, PragmasS, PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
       FromimportS, ImportexceptS, ExportS, ExportexceptS, CommentS, DiscardS,
       RaiseS, UnpackdeclS, AssumeS, AssertS, CallstrlitS, InfixS, PrefixS,
       HcallS, StaticstmtS, BindS, MixinS, UsingS, AsmS, LabS, JmpS, NoStmt:
      copyInto dest, n:
        while n.hasMore:
          trStmt c, dest, n
  else:
    dest.takeTree n

proc transformDefer*(dest: var TokenBuf; procBody: int) =
  ## Transforms a defer statement into a try-finally block.
  ## This is done early in semantic checking so other phases don't need to handle defer.
  var n = cursorAt(dest, procBody)
  assert n.stmtKind == StmtsS
  let topInfo = n.info
  var c = Context()
  var buf = createTokenBuf(50)
  # The scope id is an index into `buf` (where `trStmt`/`trDefer` build), NOT into
  # `dest`. It must be the position of the first body statement — i.e. right after
  # the `(stmts` opener `buf.takeTree` copies below — mirroring `trBlock`'s
  # `beforeBody = dest.len+1`. Seeding it with the `dest`-absolute `procBody`
  # corrupted the tree whenever enough tokens preceded a top-level `defer` (the
  # bad insert position only landed correctly while it happened to exceed `buf`'s
  # length and clamp to an append).
  let beforeBody = buf.len + 1
  c.scopeStack.add beforeBody
  buf.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      trStmt c, buf, n
  wrapScopeDefers(c, buf, beforeBody, topInfo)
  buf.addParRi()

  dest.shrink procBody
  var bc = beginRead(buf)
  while bc.hasMore:
    dest.addSubtree bc
    skip bc