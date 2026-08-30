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
## A custom pragma survives semchecking as `(pragma <sym>)`, so the validator
## reads the role where it is declared, and reads a symbol rather than a
## spelling. Where the pragma goes depends on what the routine is:
##
## * a **proc** carries it on its declaration, and the validator reads it off
##   the callee symbol of each call;
## * a **template** carries it as the first statement of its *body*, so every
##   expansion of it opens with the marker. There is no call left to read at a
##   call site — the template is gone by the time the validator sees the tree —
##   and the marker stands at the head of exactly the region it describes.
##
## The second form is what makes the roles work without `--inlineframes:on`:
## the validator needs no expansion provenance to tell which template produced
## a region, because the region says so itself. It relies on a template body
## being semchecked in the scope the template was *declared* in, so the marker
## arrives at the expansion site already bound — `std/json` wraps
## `nifcore.into`, and `into`'s body is expanded inside modules that never
## imported `nifroles`.
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

template nifEmits*(kind: string) {.pragma.}
  ## Emits exactly one child, of the named grammar kind: `"D"` (SymbolDef),
  ## `"Y"` (symbol or identifier), `"LIT"` (a literal), `"Dot"` (the empty
  ## placeholder), `"Any"` (a whole subtree whose shape is not known here), or
  ## `"None"` for a routine that is handed the buffer but adds no child of its
  ## own -- the line-info attachment behind every atom, say, which would
  ## otherwise read as an emission nobody can count.
  ##
  ## This is what lets the validator reconstruct the sequence of children a
  ## routine builds and check it against the grammar in `doc/tags.md`.

template nifOpens*() {.pragma.}
  ## Opens a tree whose tag is its second argument. What follows until the
  ## matching `{.nifCloses.}` are that tree's children.

template nifCloses*() {.pragma.}
  ## Closes the innermost tree opened by a `{.nifOpens.}` routine.
