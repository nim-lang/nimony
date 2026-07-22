## Facade for the cursor/token API.
##
## By default this is the classic `nifcursors` implementation. Under
## `-d:useNifcore` it becomes the nifcore-backed shim (`nifcore_compat`), so
## nimsem/hexer can be tried on nifcore without renaming call sites. The
## classic body now lives in `nifcursors_classic.nim`; the `include` keeps it a
## single module so nothing else changes. See doc/nifcore_shim.md.
when defined(useNifcore):
  import nifcore_compat
  export nifcore_compat
else:
  include nifcursors_classic
