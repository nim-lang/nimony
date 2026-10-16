# Native event-port/AIO ABI smoke test, independent of the I/O-ring scheduler.
when defined(illumos):
  import std/[assertions, syncio]
  import std/posix/[posix, event_port]

  proc mkstemp(path: cstring): cint {.importc: "mkstemp".}
  var name = "/tmp/nimony-eventport-XXXXXX"
  let fd = mkstemp(name.toCString)
  assert fd >= 0
  assert unlink(name.toCString) == 0
  let port = port_create()
  assert port >= 0
  var cookie: int = 123
  assert port_send(port, 42, addr cookie) == 0
  var ev = default(PortEvent)
  var timeout = Timespec(tv_sec: Time(5), tv_nsec: 0)
  assert port_get(port, addr ev, addr timeout) == 0
  assert ev.portev_source == uint16(PORT_SOURCE_USER)
  assert ev.portev_events == 42
  assert ev.portev_user == addr cookie

  # A partial batch is returned even with ETIME. Neither event can be lost.
  assert port_send(port, 1, nil) == 0
  assert port_send(port, 2, nil) == 0
  var batch = default(array[4, PortEvent])
  var count = cuint(3)
  var immediate = default(Timespec)
  let batchResult = port_getn(port, addr batch[0], 4, addr count, addr immediate)
  assert batchResult == 0 or (batchResult == -1 and errno() == ETIME), "partial port_getn"
  assert count == 2, "partial batch count"

  var pn = PortNotify(portnfy_port: port, portnfy_user: addr cookie)
  var cb = default(AioCb)
  var data = "event-port aio"
  cb.aio_fildes = fd
  cb.aio_buf = data.toCString
  cb.aio_nbytes = csize_t(data.len)
  cb.aio_offset = Off(3)
  cb.aio_sigevent.sigev_notify = SIGEV_PORT
  cb.aio_sigevent.sigev_value = addr pn
  assert aio_write(addr cb) == 0
  assert port_get(port, addr ev, addr timeout) == 0
  assert ev.portev_source == uint16(PORT_SOURCE_AIO)
  assert ev.portev_object == cast[uint](addr cb), "AIO object cookie"
  assert ev.portev_user == addr cookie
  assert aio_error(addr cb) == 0
  assert aio_return(addr cb) == data.len, "AIO write length"

  var input = default(array[32, char])
  cb = default(AioCb)
  cb.aio_fildes = fd
  cb.aio_buf = addr input[0]
  cb.aio_nbytes = csize_t(input.len)
  cb.aio_offset = Off(3)
  cb.aio_sigevent.sigev_notify = SIGEV_PORT
  cb.aio_sigevent.sigev_value = addr pn
  assert aio_read(addr cb) == 0
  assert port_get(port, addr ev, addr timeout) == 0
  assert aio_error(addr cb) == 0
  assert aio_return(addr cb) == data.len, "AIO short read at EOF"
  for i in 0..<data.len: assert input[i] == data[i]
  assert close(fd) == 0
  assert close(port) == 0

import std/syncio
echo "event ports ok"
