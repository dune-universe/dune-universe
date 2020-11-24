## v3.0.1 (2020-11-22)
- OPAM linting

## v3.0 (2020-11-22)
- MariaDB compatibility
- Dunification
- PPX Bitstring
- Implements reset connection

## v2.0 (2016-02-10)
This new release introduces backward incompatible changes:
- the old `native_data` type is now private, you have to use the new `data_*` and `to_ocaml_*` functions to convert the data between OCaml world and MySQL world.
- `get_result_set` function has a new signature: `val get_result_set : result -> Mp_result_set_packet.result_select`
- `insert_id` is now a tuple of type `(Int64.t * Big_int.big_int)`. The `Int64` value must be used when the `auto_increment` field is not a `BIGINT UNSIGNED`, otherwise the `Big_int` value must be used.

## v1.1 (2015-10-28)
- Add opam file

## v1.0 (2014-10-18)
- Initial version
