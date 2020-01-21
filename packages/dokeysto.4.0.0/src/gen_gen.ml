
(* Generic key and generic value to/from string using Marshal.
 *
 * Most users will want to pass this module to parameterize one of
 * the provided functors. *)

let string_of_key (k: 'k): string =
  Marshal.(to_string k [No_sharing])

let key_of_string (s: string): 'k =
  Marshal.from_string s 0

let string_of_value (v: 'v): string =
  Marshal.(to_string v [No_sharing])

let value_of_string (s: string): 'v =
  Marshal.from_string s 0
