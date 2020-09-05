(* Ubase,
   Conversion from UTF8 latin letters to their base character. *)
(* San Vu Ngoc, 2019 *)

(*
   A similar OCaml library: https://github.com/geneweb/unidecode

   More complete here:
   https://metacpan.org/pod/Text::Unaccent::PurePerl

   They have 4013 bindings (including 1239 that return ascii), because they
   handle other alphabets like greek, cyrillic, and chars like 1/2, 1/4,
   etc... Our list has "only" 1908 bindings.

   There is also;
   https://metacpan.org/release/Text-Transliterator/source/lib/Text/Transliterator/Unaccent.pm
   They use a fully automatically generated map based on decomposed
   normalization, hence they miss some letters like "Đ" ==> "D", because "Đ"
   decomposes to itself. *)


module Int = struct type t = int let compare : int -> int -> int = compare end
module Imap = Map.Make(Int)
module Iset = Set.Make(Int)

let latin_uchar_to_base_map =
  let add map (k, v) = Imap.add k v map in
  let map1 = List.fold_left add Imap.empty Ubase_data.latin_uchar_to_base_alist in
  List.fold_left add map1 Ubase_custom.misc_to_ascii_alist

(* Convert a latin utf8 char to a string which represents is base equivalent.
   For instance, for the letter "é", [uchar_to_string (Uchar.of_int 0xe8) =
   "e"].

    [uchar_to_string u] and [u] exactly represent the same char if and only if
   [u] is ascii (code <= 127).

   Raises [Not_found] if the uchar is not recognized as a latin letter with
   diacritic. *)
let uchar_to_string u =
  let x = Uchar.to_int u in
  if x <= 126 then Char.chr x |> String.make 1
  else Imap.find (Uchar.to_int u) latin_uchar_to_base_map

let uchar_replacement u =
  Imap.find_opt (Uchar.to_int u) latin_uchar_to_base_map

let string_to_char ?(unknown='?') s =
  if String.length s > 2 then unknown
  else s.[0]

(* Convert a Uchar to its base character (char), or the [unknown] char *)
(* note that the "ae" letter => 'a', "oe" => 'o', etc. *)
let uchar_to_char  ?(unknown='?') u =
  string_to_char ~unknown (uchar_to_string u)

(* Deprecated because too slow, see [from_utf8] below. *)
let from_utf8_old ?(malformed="?") ?strip s =
  let b = Buffer.create (String.length s) in
  let folder () _ = function
    | `Malformed  _ -> Buffer.add_string b malformed
    | `Uchar u ->
      try Buffer.add_string b (uchar_to_string u)
      with Not_found -> match strip with
        | None -> Uutf.Buffer.add_utf_8 b u (* or [Buffer.add_utf_8_uchar b u]
                                               for ocaml >*= 4.0.6 *)
        | Some strip ->  Buffer.add_string b strip
  in
  Uutf.String.fold_utf_8 folder () s;
  Buffer.contents b

(* Using options in the main function is quite faster than exceptions:
   [uchar_to_string] ==> Test Vietnamese ==> number per sec = 27324
   [uchar_to_string_opt] ==> Test Vietnamese ==> number per sec = 36666
   ==> 34% improvement !
   Even better with French test (less accents).
   Isolating the strip function ==> 37500
*)
let from_utf8 ?(malformed="?") ?strip s =
  let b = Buffer.create (String.length s) in
  let strip = match strip with
    | None -> Uutf.Buffer.add_utf_8 b
    | Some strip -> fun _ -> Buffer.add_string b strip in
  let folder () _ = function
    | `Malformed  _ -> Buffer.add_string b malformed
    | `Uchar u ->
      if Uchar.to_int u <= 127
      then Uutf.Buffer.add_utf_8 b u
      else match uchar_replacement u with
      | Some t -> Buffer.add_string b t
      | None -> strip u
  in
  Uutf.String.fold_utf_8 folder () s;
  Buffer.contents b

(* For compatibility with older API *)
let from_utf8_string = from_utf8

(* Utilities *)

(* From https://erratique.ch/software/uutf/doc/Uutf.html#examples *)

let recode ?nln ?encoding out_encoding
    (src : [`Channel of in_channel | `String of string])
    (dst : [`Channel of out_channel | `Buffer of Buffer.t])
  =
  let rec loop d e = match Uutf.decode d with
  | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e
  | `End -> ignore (Uutf.encode e `End)
  | `Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e
  | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding src in
  let e = Uutf.encoder out_encoding dst in
  loop d e

(******)

(* convert iso_8859_1 to NFC utf8 *)
let isolatin_to_utf8 s =
  let encoding = `ISO_8859_1 in
  let b = Buffer.create ((String.length s)*2) in
  recode ~encoding `UTF_8 (`String s) (`Buffer b);
  Buffer.contents b;;

let white_space_set = Iset.of_list Ubase_data.white_space

let is_space u =
  Iset.mem (Uchar.to_int u) white_space_set
