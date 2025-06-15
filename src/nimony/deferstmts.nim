#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Transforms defer statements into try-finally blocks.

import std / [assertions]
include nifprelude
import nimony_model, programs

type
  ActionItem = object
    id: int
    action: TokenBuf
  Context = object
    scopeStack: seq[int]
    actionStack: seq[ActionItem]

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let beforeBody = dest.len
  c.scopeStack.add beforeBody
  if n.stmtKind in {ScopeS, StmtsS}:
    dest.takeToken n
    while n.kind != ParRi:
      trStmt c, dest, n
    inc n # skip ParRi
  else:
    dest.addParLe(StmtsS, n.info)
    trStmt c, dest, n
  if c.actionStack.len > 0 and c.actionStack[^1].id == beforeBody:
    let a = c.actionStack.pop
    dest.add a.action
  dest.addParRi()
  discard c.scopeStack.pop

proc trDefer(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let mine = c.scopeStack[^1]

  dest.insert [parLeToken(TryS, n.info)], mine
  var fin = createTokenBuf(50)
  fin.addParLe(FinU, n.info)
  inc n
  trStmt c, fin, n
  fin.takeParRi n
  fin.addParRi() # close try statement
  c.actionStack.add ActionItem(id: mine, action: ensureMove fin)

proc trStmt(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol, SymbolDef, UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
    dest.takeToken n
  of ParLe:
    case n.stmtKind
    of ProcS, FuncS, IteratorS, ConverterS, MethodS, TemplateS, MacroS, TypeS:
      dest.takeTree n
    of IfS:
      dest.takeToken n # if
      while true:
        let k = n.substructureKind
        if k == ElifU:
          dest.takeToken n # elif
          trStmt c, dest, n
          trBlock c, dest, n
          dest.takeParRi n
        elif k == ElseU:
          dest.takeToken n # else
          trBlock c, dest, n
          dest.takeParRi n
        else:
          break
      dest.takeParRi n
    of CaseS:
      dest.takeToken n # case
      trStmt c, dest, n # subject
      while true:
        let k = n.substructureKind
        if k == OfU:
          dest.takeToken n # elif
          trStmt c, dest, n
          trBlock c, dest, n
          dest.takeParRi n
        elif k == ElifU:
          dest.takeToken n # elif
          trStmt c, dest, n
          trBlock c, dest, n
          dest.takeParRi n
        elif k == ElseU:
          dest.takeToken n # else
          trBlock c, dest, n
          dest.takeParRi n
        else:
          break
      dest.takeParRi n
    of ForS:
      dest.takeToken n # for
      trStmt c, dest, n # iterator
      trStmt c, dest, n # variables
      trBlock c, dest, n
      dest.takeParRi n
    of TryS:
      dest.takeToken n # try
      trBlock c, dest, n
      while n.substructureKind == ExceptU:
        dest.takeToken n # except
        trStmt c, dest, n
        trBlock c, dest, n
        dest.takeParRi n
      if n.substructureKind == FinU:
        dest.takeToken n # finally
        trBlock c, dest, n
        dest.takeParRi n
      dest.takeParRi n
    of WhileS, BlockS:
      dest.takeToken n # while
      trStmt c, dest, n # condition or label
      trBlock c, dest, n
      dest.takeParRi n
    of DeferS:
      trDefer c, dest, n
    else:
      dest.takeToken n
      while n.kind != ParRi:
        trStmt c, dest, n
      dest.takeParRi n
  of ParRi:
    raiseAssert "unexpected ParRi"

proc transformDefer*(dest: var TokenBuf; beforeDefer: int) =
  ## Transforms a defer statement into a try-finally block.
  ## This is done early in semantic checking so other phases don't need to handle defer.
  var n = cursorAt(dest, beforeDefer)
  assert n.stmtKind == StmtsS
  var c = Context()
  c.scopeStack.add beforeDefer
  var buf = createTokenBuf(50)
  trStmt c, buf, n
  while c.actionStack.len > 0:
    let a = c.actionStack.pop
    buf.add a.action
  dest.endRead
  dest.shrink beforeDefer
  dest.add buf
