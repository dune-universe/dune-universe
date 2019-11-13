open Utils

(** the different kinds of litteral corresponding to the (Pconst_int) ast node *)
type integer_kind =
  | Native (* 1n *)
  | Int32  (* 1l *)
  | Int64  (* 1L *)
  | Int    (* 1  *)

(** the different kinds of base for litteral numbers *)
(* WARNING: floats can only be in Hexa or Decimal *)
type base_kind =
  | Hexadecimal    (* 11 *)
  | Decimal        (* 17 *)
  | Octal          (* 0o21 *)
  | Binary         (* 0b10001 *)

(** the different kind of notation for floats *)
type float_notation =
  | Regular of string            (* 100.5 *)
  | Scientific of string*string  (* 1.005e2 *)

(** raised when a string does not match the format defined by OCaml's
   lexer for floats and integer litterals *)
exception BadFormat of string

(** error msg utility *)
let bad_prefix s =
  let msg = "unrecognized prefix for litterals:" in
  Format.asprintf "%s %s" msg s

(** error msg utility *)
let bad_suffix s =
  let msg = "unrecognized suffix for litterals:" in
  Format.asprintf "%s %c" msg s

(** Removes all occurences of the character '_'  *)
let remove__ s =
  let nb_occ = ref 0 in
  String.iter (function '_' -> incr nb_occ | _ -> ()) s;
  let s' = Bytes.make (String.length s - !nb_occ) ' ' in
  let nb_cur = ref 0 in
  String.iteri (fun i c -> if c = '_' then incr nb_cur
                           else Bytes.set s' (i- !nb_cur) c) s;
  Bytes.to_string s'

(** Categorizes a non-empty string into an integer_kind. Returns the
   pair formed by the string where the suffixes (n,l,L) where removed
   if present, and the corresponding integer_kind *)
let categorize_repr int_str =
  let before_last_idx = String.length int_str -1 in
  match int_str.[before_last_idx] with
  | 'n' -> (String.sub int_str 0 before_last_idx), Native
  | 'l' -> (String.sub int_str 0 before_last_idx), Int32
  | 'L' -> (String.sub int_str 0 before_last_idx), Int64
  | '0'..'9' | '_' -> int_str, Int
  | x -> raise (BadFormat (bad_suffix x))

(** categorizes a non-empty string into a base. Returns the pair
   formed by the string where the prefixes (0o,0x,0b) where removed if
   present, and the corresponding base *)
let categorize_base str =
  let size = String.length str in
  if size > 2 then
    match str.[0],str.[1] with
    | '0',('x'|'X') -> String.sub str 2 (size-2),Hexadecimal
    | '0',('o'|'O') -> String.sub str 2 (size-2),Octal
    | '0',('b'|'B') -> String.sub str 2 (size-2),Binary
    | ('0'..'9' | '_'),('0'..'9' | '_' | '.') -> str,Decimal
    | _ -> raise (BadFormat (bad_prefix str))
  else str,Decimal

(** Given a non-empty string representation of a float, and a base
   (either hexa or decimal), computes the associated float_notation *)
let categorize_notation str base =
  let char_exp =
    match base with
    | Hexadecimal -> 'P'
    | Decimal -> 'E'
    | _ -> assert false
  in
  match String.split_on_char char_exp str with
  | [s] -> Regular s
  | [mant;exp] -> Scientific (mant,exp)
  | _ -> assert false

let i_of_char = function
  | '0'..'9' as x -> int_of_char x - 48
  | 'a'..'f' as x -> int_of_char x - 87
  | 'A'..'F' as x -> int_of_char x - 55
  | c -> failwith (Format.asprintf "%c is not a valid numerical character" c)

let q_of_char x = i_of_char x |> Q.of_int

let int_of_base = function
  | Hexadecimal -> 16
  | Decimal -> 10
  | Octal -> 8
  | Binary -> 2

(** Computes exactly the rational corresponding to the litteral in the
   given base. Works for both integers and floats, for all bases *)
let parse_base b lit =
  let q_base = Q.of_int b in
  let size_lit = String.length lit in
  let integer_part,decimal_part =
    try
      let dot =  String.index lit '.' in
      (String.sub lit 0 dot),(String.sub lit (dot+1) (size_lit-dot-1))
    with Not_found -> lit,""
  in
  let i,_ = string_fold (fun (acc,i) c ->
                (Q.add (Q.mul (q_of_char c) i) acc),(Q.mul i q_base)
              ) (Q.zero,Q.one) (string_rev integer_part) in
  let d,_ = string_fold (fun (acc,i) c ->
                (Q.add (Q.div (q_of_char c) i) acc),(Q.mul i q_base)
              ) (Q.zero,q_base) decimal_part in
  Q.add i d

(** Computes exactly the rational corresponding to the litteral in the
   given base. Works for both integers and floats, for all bases, and
   for both scientific and regular notation *)
let parse_mant_exp b lit =
  let pred_c = match b with
  | Hexadecimal -> (function 'P' | 'p' -> true | _ -> false)
  | Decimal -> (function 'E' | 'e' -> true | _ -> false)
  | _ -> (fun _ -> false)
  in
  match split_on lit pred_c with
  | [mant;exp] ->
     let i_base = int_of_base b in
     let mant = parse_base i_base mant in
     let exp = parse_base i_base exp in
     let exp_int = Q.to_int exp in
     let exp_z = Z.pow (Z.of_int i_base) exp_int in
     Q.mul mant (Q.of_bigint exp_z)
  | [regular] -> parse_base (int_of_base b) regular
  | _ -> assert false

(** builds the rationnal corresponding to a string following OCaml's
   lexical conventions :
	|  (0…9) { 0…9 ∣  _ }
 	|	 (0x) (0…9∣ A…F∣ a…f) { 0…9∣ A…F∣ a…f∣ _ }
 	|	 (0o) (0…7) { 0…7∣ _ }
 	|	 (0b) (0…1) { 0…1∣ _ } *)
let positive_rat_of_string litteral =
  let str,base = categorize_base litteral in
  let normalized_litteral = remove__ str in
  let b = match base with
  | Hexadecimal -> 16
  | Decimal     -> 10
  | Octal       -> 8
  | Binary      -> 2
  in parse_base b normalized_litteral

(** builds the rationnal corresponding to a string following OCaml's
   lexical conventions :
	| [-] (0…9) { 0…9 ∣  _ }
 	|	[-] (0x) (0…9∣ A…F∣ a…f) { 0…9∣ A…F∣ a…f∣ _ }
 	|	[-] (0o) (0…7) { 0…7∣ _ }
 	|	[-] (0b) (0…1) { 0…1∣ _ } *)
let rat_of_string lit =
  if lit.[0] = '-' then
    Q.neg (positive_rat_of_string (String.sub lit 1 (String.length lit -1)))
  else (positive_rat_of_string lit)

(** builds an 'of_string' that convert a string representation of a
   value of a numeric type, to its corresponding value, while
   indicating if a loss of precision occured during the conversion *)
let build_os num_of_string q_of_num x =
  let i = num_of_string x in
  let r = rat_of_string x in
  match i with
  | None -> Error i
  | Some i ->
     let ir = q_of_num i in
     if ir = r then Ok i
     else Error (Some i)

let exact_int_of_string    = build_os int_of_string_opt Q.of_int
let exact_int32_of_string  = build_os Int32.of_string_opt Q.of_int32
let exact_int64_of_string  = build_os Int64.of_string_opt Q.of_int64
let exact_native_of_string = build_os Nativeint.of_string_opt Q.of_nativeint
let exact_float_of_string  = build_os float_of_string_opt Q.of_float

(** exactish string_of_float *)
let exact_string_of_float f =
  (* overkill precision than trail ending zeros *)
  Format.asprintf "%.1000f" f |> trail_ending_zeros
