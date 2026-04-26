## Conditional imports must be evaluated by the dependency analyser:
## a `when defined(SOMETHING):` import where `SOMETHING` is *not* set
## must not pull the imported module into the dep graph. We import a
## module that does not exist on disk to prove the import is dropped
## entirely (resolution would otherwise fail loudly).

when defined(this_is_definitely_not_set):
  import deps/mnonexistent_helper

when defined(this_is_definitely_not_set):
  echo "should not be reachable"
else:
  import std / [syncio]
  echo "ok"
