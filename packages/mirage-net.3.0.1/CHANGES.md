### v3.0.1 (2019-11-04)

- provide deprecated Mirage_net_lwt for smooth transition (#23 @hannesm)

### v3.0.0 (2019-10-22)

- remove mirage-net-lwt (#21 @hannesm)
- specialise to Lwt.t, Cstruct.t, Macaddr.t (#21 @hannesm)
- raise lower OCaml bound to 4.06.0 (#21 @hannesm)

### v2.0.0 (2019-02-24)

- Improvements to the write path (#15, #18 @hannesm)
  * remove `page_aligned_buffer` and `io-page` dependency
  * remove `writev`
  * provide `mtu : t -> int`
  * adjust `write : t -> size:int -> (buffer -> int) -> (unit, error) result io`
   -> allocation is done by the mirage-net implementation, and the buffer is
      filled by the upper layers. once filled, it is send out.
- Port build to dune from jbuilder (#16 @avsm)
- Switch to dune-release instead of topkg (#16 @avsm)

### v1.2.0 (2019-01-10)

- Use macaddr opam package (since ipaddr 3.0.0, macaddr is a separate opam package)
- Port to opam2

### v1.1.1 (2017-06-29)

- Fix an issue with linking with the `Result` module on 4.04 on Linux.

### v1.1.0 (2017-05-30)

- Port to [Jbuilder](https://github.com/janestreet/jbuilder).

### v1.0.0 (2016-12-26)

- Add Stats module from `mirage-net-xen` for common use
- Initial version, code imported from <https://github.com/mirage/mirage>
