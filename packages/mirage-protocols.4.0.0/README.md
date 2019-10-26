## mirage-protocols — MirageOS signatures for network protocols

mirage-protocols provides a set of module types which libraries intended to be used as MirageOS network implementations should implement.

The set of protocols defined is:

[Mirage_protocols.ETHERNET](ethernet)
[Mirage_protocols.ARP](arp)
[Mirage_protocols.IP](ip), via [Mirage_protocols.IPV4](ipv4) and [Mirage_protocols.IPV6](ipv6)
[Mirage_protocols.ICMP](icmp), via [Mirage_protocols.ICMPV4](icmpv4) and [Mirage_protocols.ICMPV6](icmpv6)
[Mirage_protocols.UDP](udp), via [Mirage_protocols.UDPV4](udpv4) and [Mirage_protocols.UDPV6](udpv6)
[Mirage_protocols.TCP](tcp), via [Mirage_protocols.TCPV4](tcpv4) and [Mirage_protocols.TCPV6](tcpv6)

mirage-protocols is distributed under the ISC license.

[ethernet](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-ETHERNET/index.html)
[arp](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-ARP/index.html)
[ip](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-IP/index.html)
[ipv4](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-IPV4/index.html)
[ipv6](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-IPV6/index.html)
[icmp](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-ICMP/index.html)
[icmpv4](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-ICMPV4/index.html)
[icmpv6](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-ICMPV6/index.html)
[udp](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-UDP/index.html)
[udpv4](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-UDPV4/index.html)
[udpv6](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-UDPV6/index.html)
[tcp](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-TCP/index.html)
[tcpv4](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-TCPV4/index.html)
[tcpv6](https://mirage.github.io/mirage-protocols/mirage-protocols/Mirage_protocols/module-type-TCPV6/index.html)

## Installation

mirage-protocols can be installed with `opam`:

    opam install mirage-protocols

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
mirage-protocols`.

[doc]: https://mirage.github.io/mirage-protocols/
