# Helper for `timportshapes`. Everything here exists to be compiled as an
# IMPORT — see that test for why the position, not the code, is the point.
import std / [syncio]

proc splitAt*(p: string): int =
  ## A `splitFile`-shaped scan: two locals written inside a `for` whose body is
  ## an `if`/`elif` chain with a `break`. Both stores were lost — the proc read
  ## back the values the locals were INITIALIZED to — when a peephole deleted
  ## the instruction that materialized one of them, having asked whether its
  ## scratch register was dead of the whole BUFFER rather than of this proc.
  ## Every proc's first scratch is named `tmp0.0`, so the answer came from the
  ## next proc's binding. Both branches and the `break` are load-bearing:
  ## dropping any one of them stops the fold from being offered at all.
  var pathEnd = -1
  var extPos = p.len
  for i in countdown(p.len-1, 1):
    if p[i] == '.':
      if extPos == p.len: extPos = i
    elif p[i] == '/':
      pathEnd = i
      break
  echo "split: ", pathEnd, " ", extPos
  result = pathEnd

template defineDoubler() {.untyped.} =
  ## A routine a template GENSYMS. Nothing outside this module can name it, but
  ## it is still a module-level declaration and the backends emit it as one —
  ## so it needs a module-qualified name to appear in the module's index.
  ## Named `doubler.1` (local layout) it was absent from that index, and a
  ## native build of an importer could not resolve the call this module's own
  ## init makes to it.
  proc doubler(a: int): int =
    result = a * 2
  echo "doubled: ", doubler(21)

defineDoubler()

# Called from this module's OWN init as well as from the importer: what the
# peephole did depended on what followed the fold in the buffer, and the module
# init is what follows.
discard splitAt("foo.nim/bar.nim")

template defineAnswer() =
  let answer {.inject.} = 42
  ## Same story for a `let`: expanded here it is a module-level global, and a
  ## local-layout name kept it out of the index too.

defineAnswer()

proc theAnswer*(): int = answer
