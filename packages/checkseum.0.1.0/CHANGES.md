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
