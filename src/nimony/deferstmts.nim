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
    action: TokenBuf
  Context = object
    scopeStack: seq[int]
    actionStack: seq[ActionItem]
    retSym: SymId

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)
  {.ensuresNif: addedAny(dest).}

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let beforeBody = dest.len+1
  c.scopeStack.add beforeBody
  if n.stmtKind in {ScopeS, StmtsS}:
    dest.add n
    n.into:
      while n.hasMore:
        trStmt c, dest, n
  else:
    dest.addParLe(StmtsS, n.info)
    trStmt c, dest, n
  while c.actionStack.len > 0 and c.actionStack[^1].id == beforeBody:
    let a = c.actionStack.pop
    dest.add a.action
  dest.addParRi()
  discard c.scopeStack.pop

proc trDefer(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let mine = c.scopeStack[^1]

  dest.insert [parLeToken(TryS, n.info), parLeToken(StmtsS, n.info)], mine
  var fin = createTokenBuf(50)
  fin.addParRi() # stmts body from try statement
  fin.addParLe(FinU, n.info)
  n.into: # enter (defer
    trStmt c, fin, n
  fin.addParRi() # close the finally clause
  fin.addParRi() # close try statement
  c.actionStack.add ActionItem(id: mine, action: ensureMove fin)

proc trReturn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if c.retSym != NoSymId and not (n.firstSon.kind == Symbol and n.firstSon.symId == c.retSym):
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
  case n.kind
  of Symbol, SymbolDef, UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeToken n
  of ParLe:
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
            break
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
            break
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
        assert n.kind == SymbolDef
        c.retSym = n.symId
        dest.takeToken n
        while n.hasMore:
          trStmt c, dest, n
    of CallS, CmdS, GvarS, TvarS, VarS, ConstS, GletS, TletS, LetS, CursorS,
       PatternvarS, EmitS, AsgnS, ScopeS, WhenS, BreakS, ContinueS, YldS,
       StmtsS, PragmasS, PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
       FromimportS, ImportexceptS, ExportS, ExportexceptS, CommentS, DiscardS,
       RaiseS, UnpackdeclS, AssumeS, AssertS, CallstrlitS, InfixS, PrefixS,
       HcallS, StaticstmtS, BindS, MixinS, UsingS, AsmS, NoStmt:
      copyInto dest, n:
        while n.hasMore:
          trStmt c, dest, n
  of ParRi:
    raiseAssert "unexpected ParRi"

proc transformDefer*(dest: var TokenBuf; procBody: int) =
  ## Transforms a defer statement into a try-finally block.
  ## This is done early in semantic checking so other phases don't need to handle defer.
  var n = cursorAt(dest, procBody)
  assert n.stmtKind == StmtsS
  var c = Context()
  var buf = createTokenBuf(50)
  # The scope id is an index into `buf` (where `trStmt`/`trDefer` build), NOT into
  # `dest`. It must be the position of the first body statement — i.e. right after
  # the `(stmts` opener `buf.takeToken` copies below — mirroring `trBlock`'s
  # `beforeBody = dest.len+1`. Seeding it with the `dest`-absolute `procBody`
  # corrupted the tree whenever enough tokens preceded a top-level `defer` (the
  # bad insert position only landed correctly while it happened to exceed `buf`'s
  # length and clamp to an append).
  c.scopeStack.add buf.len + 1
  buf.add n
  n.into:
    while n.hasMore:
      trStmt c, buf, n
  while c.actionStack.len > 0:
    let a = c.actionStack.pop
    buf.add a.action
  buf.addParRi()

  dest.endRead
  dest.shrink procBody
  dest.add buf
