external format_float: string -> float -> string
  = "caml_format_float"
external format_int: string -> int -> string
  = "caml_format_int"
external format_int32: string -> int32 -> string
  = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
  = "caml_nativeint_format"
external format_int64: string -> int64 -> string
  = "caml_int64_format"

(* borrowed from ocaml/stdlib/printf.ml with a bug fix *)

(* Format a float argument as a valid Caml lexem. *)
let format_float_lexem =
  let valid_float_lexem s =
    let l = String.length s in
    if l = 0 then "nan" else
      let add_dot s =
        if s.[0] = ' ' || s.[0] = '+' || s.[0] = '0'
        then String.sub s 1 (l - 1) ^ "."
        else String.sub s 0 l ^ "." in

      let rec loop i =
        if i >= l then add_dot s else
        match s.[i] with
        | '.' -> s
        | _ -> loop (i + 1) in

      loop 0 in

   (fun sfmt x ->
    let s = format_float sfmt x in
    match classify_float x with
    | FP_normal | FP_subnormal | FP_zero -> valid_float_lexem s
    | FP_nan | FP_infinite -> s)
;;

let format_camlfloat = format_float_lexem

type 'a out = {
  string : string -> unit;
  char : char -> unit;
  flush : unit -> unit;
  finish : unit -> 'a;
}

type ('a, 'b) t = 'b out -> 'a

module Buffer : sig
  type t
  val create : unit -> t
  val add_string : t -> string -> unit
  val add_char : t -> char -> unit
  val contents : t -> string
  (* val length : t -> int *) (* not used outside *)
  val _clear : t -> unit
end = struct
  type elem = Bytes of bytes | Char of char 
  type t = elem list ref
  let create () = ref []
  let add_string t s = t := Bytes (Bytes.of_string s) :: !t
  let add_char t c = t := Char c :: !t
  let _clear t = t := []

  let elem_length = function
      | Bytes s -> Bytes.length s
      | Char _ -> 1

  let length t =
    List.fold_left (fun acc e -> acc + elem_length e) 0 !t

  let contents t = 
    let len = length t in
    let buf = Bytes.create len in
    assert (List.fold_left (fun pos -> function
      | Bytes s -> 
	  let len = Bytes.length s in
	  let pos = pos - len in
	  Bytes.unsafe_blit s 0 buf pos len;
	  pos
      | Char c ->
	  let pos = pos - 1 in
	  Bytes.unsafe_set buf pos c;
	  pos) len !t = 0);
    Bytes.to_string buf
end

module Sprintf = struct
  let create size =
    let buf = Buffer.create size in
    { string = Buffer.add_string buf;
      char = Buffer.add_char buf;
      flush = (fun () -> ());
      finish = (fun () -> Buffer.contents buf) }
  let exec init fmt = fmt (create init)
end

let sprintf (fmt : ('a, string) t) = Sprintf.exec () fmt
