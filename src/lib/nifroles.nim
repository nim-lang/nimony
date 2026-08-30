#
#
#           Nimony Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## What a NIF traversal/emission routine does to the cursor and the buffer,
## declared by the routine itself.
##
## The validator has to know that `skip n` advances without emitting while
## `takeTree dest, n` does both, that `takeLocal` consumes a whole declaration
## and that `copyInto` opens a tree and closes it again. It used to know that
## from a table of ~50 names, which is knowledge kept at the wrong end: adding
## an operation to the API meant editing the validator, and a pass-local proc
## that happened to be called `skip` was indistinguishable from this one.
##
## A custom pragma survives semchecking as `(pragma <sym>)` on the declaration,
## so the validator reads the role where it is declared — for a proc from the
## callee symbol, for a template from the symbol its expansion's provenance
## names. Both are exact: they are symbols, not spellings.
##
## Emission is deliberately *not* in this list. A routine whose first parameter
## is a `var TokenBuf` and which takes no cursor emits and nothing else; that
## is what its signature already says, and saying it again on each of the forty
## `add*` overloads would be noise that can drift.

template nifAdvance*() {.pragma.}
  ## Advances the cursor without emitting anything: the input it moves over is
  ## dropped unless something else accounts for it. Needs a `SkipIntent`
  ## argument at the call site to be considered justified.

template nifBalanced*() {.pragma.}
  ## Advances the cursor *and* emits what it moved over, so it neither drops
  ## input nor invents output.

template nifWrap*() {.pragma.}
  ## Opens a tree, runs the body it was given, closes it. Balanced at the tag
  ## level whatever the body does.

template nifReads*() {.pragma.}
  ## Consumes a structural unit (a declaration, a routine header) and returns
  ## its parts for the caller to emit later. Balanced, but only because the
  ## fields it hands back are the caller's obligation now.

template nifDelegates*() {.pragma.}
  ## Hands the cursor to another pass, which owns the obligation from there.

template nifEmits*(kind: untyped) {.pragma.}
  ## Emits exactly one child, of the given grammar kind: `D` (SymbolDef), `Y`
  ## (symbol or identifier), `LIT` (a literal), `Dot` (the empty placeholder)
  ## `Any` (a whole subtree whose shape is not known here), or `None` for a
  ## routine that is handed the buffer but adds no child of its own -- the
  ## line-info attachment behind every atom, say, which would otherwise read as
  ## an emission nobody can count. This is what
  ## lets the validator reconstruct the sequence of children a routine builds
  ## and check it against the grammar in `doc/tags.md`.

template nifOpens*() {.pragma.}
  ## Opens a tree whose tag is its second argument. What follows until the
  ## matching `{.nifCloses.}` are that tree's children.

template nifCloses*() {.pragma.}
  ## Closes the innermost tree opened by a `{.nifOpens.}` routine.
