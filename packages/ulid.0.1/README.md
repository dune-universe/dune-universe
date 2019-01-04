# ocaml-ulid

ULIDs are 
- 128-bit compatibility with UUID
1.21e+24 unique ULIDs per millisecond
- Lexicographically sortable!
- Canonically encoded as a 26 character string, as opposed to the 36 character UUID
- Uses Crockford's base32 for better efficiency and readability (5 bits per character)
- Case insensitive
- No special characters (URL safe)
- Monotonic sort order (correctly detects and handles the same millisecond)

See the full spec [here](https://github.com/ulid/spec).

This package uses [nocrypto's](https://github.com/mirleft/ocaml-nocrypto) fortuna RNG for the random part of the ULID, but a different generator can be passed in with the signature `int -> int` (to generate a random number between 0 and n-1).

Help is welcome!

## Installation
ulid can be installed with `opam`:

```bash
opam install ulid
```

## Usage
basic ulid generation:
```ocaml
let ulid = Ulid.ulid ();; (* something like 01D07HWDR77VCQ6CC8XSDC916T *)
```

passing in a seed time instead of current time (see spec for more details):
```ocaml
let ulid = Ulid.ulid ?seed_time:123 ();;
```

getting monotonic ulids:
```ocaml
let get_ulid = Ulid.monotonic_factory ();;
let ulid = get_ulid ();;
```

using a different PRNG:
```ocaml
let get_ulid = Ulid.ulid_factory ~prng:my_favorite_prng ();;
let ulid = get_ulid ();;
```

## License
MIT