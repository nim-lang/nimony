import std/[syncio]

# Regression: a `{.cursor.}` local captured by a `.closure` proc that ASSIGNS
# and READS through it crashed hexer with
#   [Bug] could not find symbol: nodeRef.0
# Lambda-lifting's capture analysis only hoisted `{ParamY, LetY, VarY, ResultY}`
# locals into the environment; a cursor local (kind CursorY) was left as a bare
# cross-proc symbol reference, which the following duplifier pass could not type.
# The cursor is load-bearing here: it breaks the node -> releaseView(closure) ->
# env -> node ref cycle, so the fix must hoist the cursor local into the env AS A
# `.cursor` field (non-owning) — otherwise the cycle re-forms and node leaks.

type
  NodeObj = object
    viewW: int
    releaseView: proc() {.closure.}
  Node = ref NodeObj

var gDestroyed = 0
proc `=destroy`(x: NodeObj) =
  inc gDestroyed

proc setup(node: Node) =
  let nodeRef {.cursor.} = node
  node.releaseView = proc() {.closure.} =
    nodeRef.viewW = nodeRef.viewW + 5   # write through the cursor
    echo "closure viewW=", nodeRef.viewW # read through the cursor

proc run() =
  let n = Node(viewW: 10)
  setup(n)
  n.releaseView()
  echo "node viewW=", n.viewW            # mutation visible on the aliased object

run()
# Exactly one destroy: the cursor env field did not inflate node's rc, so the
# sole owner (`n`) frees it once — no leak (would print 0) and no double free.
echo "destroyed=", gDestroyed
