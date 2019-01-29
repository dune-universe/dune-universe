open Core
open Freetds

type t =
  | Bignum of Bignum.t
  | Bool of bool
  | Float of float
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | String of string
  | Date of Time.t
[@@deriving sexp]

(** [recode ~src ~dst str] decodes [str] from the character set given by [~src],
    i.e. "UTF-8", "CP1252", etc. and then encodes it into the character set
    [~dst].

    We need to do character set conversions because SQL Server can't handle
    UTF-8 in any reasonable way, so most DB's are going to be using CP1252.

    If [str] is not a valid string in the [~src] encoding, we give up on nice
    conversions and just ASCIIfy the input and return it (just stripping out
    all non-ASCII characters)

    If [str] contains a character that can't be represented in [~dst], we
    skip that character and move on. *)
let recode ~src ~dst input =
  (*  Note that we originally used //TRANSLIT and //IGNORE to do this in iconv,
      but iconv is inconsistent between platforms so we do the conversion one
      char at a time. *)
  try
    let decoder = Encoding.decoder src
    and encoder = Encoding.encoder dst
    and dec_i = ref 0
    and enc_i = ref 0
    (* Using string like this is not recommended, but the library we're using
       doesn't support bytes as an input to Encoding.encode

       Note that we make a buffer with n * 4 bytes long because UTF-8 characters
       can be a maximum of 4 bytes. This is very pessimistic, but resizing a
       string constantly would be annoying and slow.

       https://stijndewitt.com/2014/08/09/max-bytes-in-a-utf-8-char/ *)
    and output = Bytes.create (String.length input * 4) |> Bytes.to_string in
    let input_len = String.length input
    and output_len = String.length output in
    while !dec_i < input_len do
      Encoding.decode decoder input !dec_i (input_len - !dec_i)
      |> function
      | Encoding.Dec_ok (c, n) ->
        dec_i := !dec_i + n;
        begin
          Encoding.encode encoder output !enc_i (output_len - !enc_i) c
          |> function
          | Encoding.Enc_ok n -> enc_i := !enc_i + n
          | Enc_error -> (* skip characters that can't be translated *) ()
          | Enc_need_more -> failwith "Encoder is out of space"
        end
      | Dec_error -> failwith "Decode error"
      | Dec_need_more -> failwith "Decoder ended with partial character"
    done;
    String.sub output ~pos:0 ~len:!enc_i
  with exn ->
    Logger.info !"Recoding error, falling back to ascii filter %{sexp: exn} %s"
      exn input;
    String.filter input ~f:(fun c -> Char.to_int c < 128)

let of_data ~month_offset data =
  match data with
  | Dblib.BIT b -> Some (Bool b)
  | INT i
  | SMALL i
  | TINY i -> Some (Int i)
  | INT32 i -> Some (Int32 i)
  | INT64 i -> Some (Int64 i)
  | FLOAT f
  | MONEY f -> Some (Float f)
  | DECIMAL s
  | NUMERIC s ->
    Some (Bignum (Bignum.of_string s))
  | BINARY s -> Some (String s)
  | STRING s ->
    Some (String (recode ~src:"CP1252" ~dst:"UTF-8" s))
  | DATETIME (y, mo, day, hr, min, sec, ms, _zone) ->
    (* FIXME: Timezones don't match in FreeTDS 0.91 and 1.0, so for now we
       just assume everything in UTC. *)
    let mo = mo + month_offset in
    let date =
      Date.create_exn ~y ~m:(Month.of_int_exn mo) ~d:day in
    let time = Time.Ofday.create ~hr ~min ~sec ~ms ~us:0 () in
    let datetime = Time.of_date_ofday date time
                     ~zone:Time.Zone.utc in
    Some (Date datetime)
  | NULL -> None

let to_string ~quote_string =
  function
  | None -> "NULL"
  | Some p ->
    match p with
    | Bignum n -> Bignum.to_string_hum n |> quote_string
    | Bool b -> if b then "1" else "0"
    | Float f -> Float.to_string f
    | Int i -> Int.to_string i
    | Int32 i -> Int32.to_string i
    | Int64 i -> Int64.to_string i
    | String s -> s |> quote_string
    | Date t ->
      Time.format ~zone:Time.Zone.utc t "%Y-%m-%dT%H:%M:%S" |> quote_string

let to_string_escaped =
  (* Quote the string by replacing ' with '' and null with CHAR(0). This
     is somewhat complicated because I couldn't find a way to escape a
     null character without closing the string and adding +CHAR(0)+.
     I couldn't do this with String.concat since that would force us to
     concat every CHAR, which is inefficient (i.e. "asdf" would be passed as
     'a'+'s'+'d'+'f'). *)
  let quote_string s =
    (* Need to convert to CP1252 since SQL Server can't handle UTF-8 in any
       reasonable way. *)
    let s = recode ~src:"UTF-8" ~dst:"CP1252" s in
    (* len * 2 will always hold the resulting string unless it has null
       chars, so this should make the standard case fast without wasting much
       memory. *)
    let buf = Buffer.create ((String.length s) * 2) in
    let in_str = ref false in
    let first = ref true in
    for i = 0 to String.length s - 1 do
      let c = String.get s i in
      if c = '\x00' then begin
        if !in_str then begin
          Buffer.add_char buf '\'';
          in_str := false;
        end;
        if not !first then
          Buffer.add_char buf '+';
        Buffer.add_string buf "CHAR(0)"
      end
      else begin
        if not !in_str then begin
          if not !first then
            Buffer.add_char buf '+';
          Buffer.add_char buf '\'';
          in_str := true;
        end;
        if c = '\'' then
          Buffer.add_string buf "''"
        else
          Buffer.add_char buf c;
      end;
      first := false;
    done;
    if !first then begin
      Buffer.add_char buf '\'';
      in_str := true
    end;
    if !in_str then
      Buffer.add_char buf '\'';
    Buffer.contents buf
  in
  to_string ~quote_string

let to_string = to_string ~quote_string:Fn.id

let with_error_msg ?column ~f type_name t =
  try
    f t
  with Assert_failure _ ->
    let column_info = match column with
      | None -> ""
      | Some column -> sprintf " column %s" column
    in
    failwithf !"Failed to convert%s %{sexp: t} to type %s"
      column_info t type_name ()

let bignum ?column =
  with_error_msg ?column "float" ~f:(function
    | Bignum b -> b
    | Float f -> Bignum.of_float_dyadic f
    | Int i -> Bignum.of_int i
    | Int32 i -> Int.of_int32_exn i |> Bignum.of_int
    | Int64 i -> Int64.to_string i |> Bignum.of_string
    | _ -> assert false)

let float ?column =
  with_error_msg ?column "float" ~f:(function
    | Bignum b -> Bignum.to_float b
    | Float f -> f
    | Int i -> Float.of_int i
    | Int32 i -> Int.of_int32_exn i |> Float.of_int
    | Int64 i -> Float.of_int64 i
    | _ -> assert false)

let int ?column =
  with_error_msg ?column "int" ~f:(function
    | Bignum b -> Bignum.to_int_exn b
    | Bool false -> 0
    | Bool true -> 1
    | Float f -> Int.of_float f
    | Int i -> i
    | Int32 i -> Int32.to_int_exn i
    | Int64 i -> Int64.to_int_exn i
    | _ -> assert false)

let int32 ?column =
  with_error_msg ?column "int32" ~f:(function
    | Bignum b -> Bignum.to_int_exn b |> Int32.of_int_exn
    | Bool false -> Int32.zero
    | Bool true -> Int32.one
    | Float f -> Int32.of_float f
    | Int i -> Int32.of_int_exn i
    | Int32 i -> i
    | _ -> assert false)

let int64 ?column =
  with_error_msg ?column "int64" ~f:(function
    | Bignum b -> Bignum.to_int_exn b |> Int64.of_int_exn
    | Bool false -> Int64.zero
    | Bool true -> Int64.one
    | Float f -> Int64.of_float f
    | Int i -> Int64.of_int i
    | Int32 i -> Int64.of_int32 i
    | Int64 i -> i
    | _ -> assert false)

let bool ?column =
  with_error_msg ?column "bool" ~f:(function
    | Bool b -> b
    (* MSSQL's native BIT type is 0 or 1, so conversions from 0 or 1 ints
       make sense *)
    | Int i when i = 0 -> false
    | Int i when i = 1 -> true
    | Int32 i when i = Int32.zero -> false
    | Int32 i when i = Int32.one -> true
    | Int64 i when i = Int64.zero -> false
    | Int64 i when i = Int64.one -> true
    | _ -> assert false)

let str ?column =
  with_error_msg ?column "string" ~f:(function
    | Bignum b -> Bignum.to_string_hum b
    | Bool b -> Bool.to_string b
    | Float f -> Float.to_string f
    | Int i -> Int.to_string i
    | Int32 i -> Int32.to_string i
    | Int64 i -> Int64.to_string i
    | String s -> s
    | Date t -> Time.to_string_abs ~zone:Time.Zone.utc t)

let date ?column =
  with_error_msg ?column "date" ~f:(function
    | Date d -> Date.of_time ~zone:Time.Zone.utc d
    | String s -> Date.of_string s
    | _ -> assert false)

let datetime ?column =
  with_error_msg ?column "datetime" ~f:(function
    | Date d -> d
    | String s -> Time.of_string_abs s
    | _ -> assert false)
