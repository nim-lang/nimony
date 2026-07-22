## Facade for the streaming/pool API.
##
## By default this is the classic `nifstreams` implementation. Under
## `-d:useNifcore` it becomes the nifcore-backed shim (`nifcore_compat`) — the
## same shim `nifcursors` selects, so the two share one set of symbols (a
## diamond re-export of identical definitions, not a conflict). The classic
## body lives in `nifstreams_classic.nim`. See doc/nifcore_shim.md.
when defined(useNifcore):
  import nifcore_compat
  export nifcore_compat
else:
  include nifstreams_classic
