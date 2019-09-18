## v1.7.0 (2019-09-16)

- Use max_buffer_size as buffer length as required by Vmnet (@magnuss)
- Update to use latest ocaml-vmnet to improve error handling (@magnuss)

## v1.6.0 (2019-02-24)

- adapt to mirage-net 2.0.0 change (@hannesm)
- use Vmnet.mtu (thanks to @avsm)

## v1.5.0 (2019-01-31)

- Switch to Dune from jbuilder (@avsm)
- Remove topkg in favour of dune-release (@avsm)
- Upgrade opam metadata to 2.0 (@hannesm @avsm)

## 1.4.0 (2017-05-19)

- Remove unnecessary mirage-dev travis remote (#22 by @yomimono)
- Switch to jbuilder (#23 by @djs55)

## 1.3.0 (2017-01-19)

- Adapt to MirageOS 3 types.
- Bugfixes for `listen`: be tail-recursive (#19, by @samoht), don't restart when canceled (#17, by @yomimono)

## 1.2.0 (2016-11-11)

- Switch to topkg (#11, @hannesm and @samoht)
- Remove camlp4 (#7, @samoht)

## 1.1.0 (2015-03-04)

- Add an explicit `connect` function to interface (#1).
- Support the `Io_page` 1.4.0+ API. (#2).

## 1.0.0 (2014-12-01)

- Initial public release.
