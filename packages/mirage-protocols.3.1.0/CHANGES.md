### v3.1.0 (2019-10-14)

- add polymorphic variant `Would_fragment to Ip.error (#20 @hannesm)
- extend ICMP.write and UDP.write with optional ttl:int argument (#21 @phaer)
- remove IP.set_ip (#20 @hannesm)

### v3.0.0 (2019-07-18)

- replace `uipaddr` with `pp_ipaddr`, since the only use is to print
  human-readable IP addresses (#18 @yomimono @linse)
- port to dune from jbuilder (#17 @hannesm)

### v2.0.0 (2019-02-24)

- Ethif/ETHIF renamed to Ethernet/ETHERNET (#16)
- Ethernet.proto defines a polymorphic variant of ethernet types (#15)
- Ip.proto defines a polymorphic variant of ip types (#15)
- Ethernet.writev is removed (#15)
- Ethernet.write expects an optional source mac address, a destination mac
  address, a protocol, an optional size and a fill function. Ethernet writes
  the Ethernet header to the buffer. (#15)
- Ip.writev and Ip.checksum are removed (#15)
- Ip.write expects an optional fragment, ttl, src, and a size and fill function,
  as well as a list of payload buffers. Size default to MTU. (#15)
- migrated build system to dune

### v1.4.1 (2019-01-10)

- ipaddr3 compatibility

### v1.4.0 (2018-09-15)

- remove unused types, since `connect` no longer in signatures (since Mirage3)
  `netif` from ETHIF
  `ethif` and `prefix` from IP
  `ip` from UDP and TCP

### v1.3.0 (2017-09-06)

- add support for TCP keepalives by changing the signature of the
  `TCP.input` function
- jbuilder is now a build dependency

### v1.2.0 (2017-06-15)

- port build to Jbuilder

### v1.1.0 (2016-03-02)

- require an mtu function in the ETHIF module type.

### v1.0.0 (2016-12-29)

- import ETHIF, ARP, IP, IPV4, IPV6, TCP, UDP, ICMP module types from mirage-types and mirage-types-lwt
