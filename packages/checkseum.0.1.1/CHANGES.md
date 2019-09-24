### v0.1.1 2019-09-12 Paris (France)

- Compatibility with mirage+dune (#29, @dinosaure)
- Use `bigarray-compat` (#29, @TheLortex)
- Add constraints with < mirage-runtime.4.0.0

  `checkseum` (as some others packages) must be used with MirageOS 4
  where `checkseum.0.9.0` is a compatibility package with Mirage)S 3

- Replace `STDC` macro check by `STDDEF_H_` to be able to compile (#34, @dinosaure)
  checkseum with +32bit compiler variant (#34, @dinosaure)
- Use a much more simpler implementation of CRC32C to be compatible with large set of targets (#34, @dinosaure)
- Avoid fancy operators in OCaml implementation of CRC32 and CRC32C (#34, @dinosaure)
- Require `optint.0.0.3` at least (#34, @dinosaure)

### v0.1.0 2019-05-16 Paris (France)

- Use experimental feature of variants with `dune` (#25, @dinosaure, review @rgrinberg)
  `checkseum` requires at least `dune.1.9.2`
- Add conflict with `< mirage-xen-posix.3.1.0` packages (#21, @hannesm)
- Provide `unsafe_*` functions (@dinosaure)
- Re-organize C implementation as `digestif` (@dinosaure)
- Remove `#include <stdio.h>` in C implementation (@dinosaure)
- Avoid partial application of functions, optimization (#19, @dinosaure)
- Add ocamlformat support (@dinosaure)
- _cross-compilation_ adjustement about MirageOS backends (#18, @hannesm)

### v0.0.3 2018-10-15 Paris (France)

- _Dunify_ project
- Add CRC32 implementation
- Fixed META file (@g2p)
- Update OPAM file

### v0.0.2 2018-08-23 Paris (France)

- Fix windows support (@nojb)

### v0.0.1 2018-07-06 Paris (France)

- First release of `checkseum`
