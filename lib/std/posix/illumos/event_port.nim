# Private include of std/posix/event_port. sys/port.h and port.h.
const
  PORT_SOURCE_AIO* = cint(1)
  PORT_SOURCE_TIMER* = cint(2)
  PORT_SOURCE_USER* = cint(3)
  PORT_SOURCE_FD* = cint(4)
  PORT_SOURCE_ALERT* = cint(5)
  PORT_SOURCE_MQ* = cint(6)
  PORT_SOURCE_FILE* = cint(7)
  PORT_ALERT_SET* = cint(1)
  PORT_ALERT_UPDATE* = cint(2)
  PORT_ALERT_INVALID* = cint(3)
  POLLIN* = cint(0x0001)
  POLLOUT* = cint(0x0004)
  POLLERR* = cint(0x0008)
  POLLHUP* = cint(0x0010)
  POLLNVAL* = cint(0x0020)

type
  PortEvent* {.pure.} = object
    portev_events*: cint
    portev_source*: uint16
    portev_pad: uint16
    portev_object*: uint
    portev_user*: nil pointer
  PortNotify* {.pure.} = object
    portnfy_port*: cint
    portnfy_user*: nil pointer

proc port_create*(): cint {.importc: "port_create", sideEffect.}
proc port_associate*(port, source: cint; obj: uint; events: cint;
                     user: nil pointer): cint {.importc: "port_associate", sideEffect.}
proc port_dissociate*(port, source: cint; obj: uint): cint {.importc: "port_dissociate", sideEffect.}
proc port_send*(port, events: cint; user: nil pointer): cint {.importc: "port_send", sideEffect.}
proc port_get*(port: cint; event: ptr PortEvent; timeout: nil ptr Timespec): cint {.importc: "port_get", sideEffect.}
proc port_getn*(port: cint; events: ptr PortEvent; max: cuint; count: ptr cuint;
               timeout: nil ptr Timespec): cint {.importc: "port_getn", sideEffect.}
proc port_alert*(port, flags, events: cint; user: nil pointer): cint {.importc: "port_alert", sideEffect.}
