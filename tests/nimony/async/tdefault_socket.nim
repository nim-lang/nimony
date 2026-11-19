# A `Socket`/`UdpSocket`/`Listener` left at its default owns no descriptor:
# its `=destroy` must not close fd 0 (stdin). Issue #3044 item 2.
import std/[syncio, socket]

type
  Conn = object
    name: string
    sock: Socket

proc main() =
  let c = Conn(name: "x")
  echo c.sock.fd
  echo default(Socket).fd, " ", default(UdpSocket).fd, " ", default(Listener).fd

main()
