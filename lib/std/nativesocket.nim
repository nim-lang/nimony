type
  Port* = distinct uint16 ## port type

  Domain* = enum ## \
    ## domain, which specifies the protocol family of the
    ## created socket. Other domains than those that are listed
    ## here are unsupported.
    AF_UNSPEC = 0, ## unspecified domain (can be detected automatically by
                   ## some procedures, such as getaddrinfo)
    AF_UNIX = 1,   ## for local socket (using a file). Unsupported on Windows.
    AF_INET = 2,   ## for network protocol IPv4 or
    AF_INET6 = when defined(macosx): 30 elif defined(windows): 23 elif defined(illumos): 26 elif defined(freebsd): 28 else: 10 ## for network protocol IPv6.

  Protocol* = enum      ## third argument to `socket` proc; the ordinals are
                        ## the IANA protocol numbers every platform uses
    IPPROTO_IP = 0,     ## Internet protocol.
    IPPROTO_ICMP = 1,   ## Internet Control message protocol.
    IPPROTO_TCP = 6,    ## Transmission control protocol.
    IPPROTO_UDP = 17,   ## User datagram protocol.
    IPPROTO_IPV6 = 41,  ## Internet Protocol Version 6.
    IPPROTO_ICMPV6 = 58, ## Internet Control message protocol for IPv6.
    IPPROTO_RAW = 255   ## Raw IP Packets Protocol. Unsupported on Windows.

when defined(illumos):
  type SockType* = enum
    SOCK_DGRAM = 1, SOCK_STREAM = 2, SOCK_RAW = 4, SOCK_SEQPACKET = 6
else:
  type SockType* = enum
    SOCK_STREAM = 1, SOCK_DGRAM = 2, SOCK_RAW = 3, SOCK_SEQPACKET = 5
