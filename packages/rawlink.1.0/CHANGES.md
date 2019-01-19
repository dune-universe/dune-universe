## v1.0 (2019-01-17)

This release splits the Lwt package into a separate
`rawlink.lwt` ocamlfind package.  Existing users of `Lwt_rawlink`
can just rename the ocamlfind package `rawlink` to `rawlink.lwt`
to get the previous functionality.

* Add a `dhcp_client_filter` for DHCP client port (#8 by @yomimono)
* Rename `dhcp_filter` to `dhcp_server_filter` (#8 by @yomimono)
* Support Lwt 4.0 (#10 by @hannesm)
* Port build system to Dune (#11 by @avsm)

## 0.7 (2017-11-26)

* Fix compilation on POSIX-like systems (such as FreeBSD) by including netinet/in.h

## 0.6 (2017-11-24)

* Support ocaml 4.06
* Fix cstruct linking

## 0.5 (2017-04-16)

* Convert to topkg

## 0.4 (2016-12-17)

* Convert to ppx

## 0.3 (2015-08-30)

* Fix fd leak in bpf_open
* Fix linux send function

## 0.2 (2015-08-28)

* Fix to bpf_split_buf

## 0.1 (2015-10-09)

* Initial release
