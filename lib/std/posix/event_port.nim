## Event ports and their POSIX AIO notification interface.
## Platform declarations are header-free and selected here so consumers need
## not depend on illumos internals. Oracle Solaris can add its own ABI later.
when defined(illumos):
  from ./posix import Timespec, Off
  include "illumos/event_port"
  include "illumos/aio"
