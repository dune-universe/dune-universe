open Camlon
open Typerep_lib.Std
open Ppx_sexp_conv_lib.Conv

module Q = QCheck
module QT = QCheck_of_typerep

type longident = Longident.t =
  | Lident of XGen.ident
  | Ldot of longident * XGen.ident
  | Lapply of longident * longident

and t' = t

and t = Ocaml.t =
  | Bool of bool
  | Int31       of int (** int for 32 bit arch *)
  | Int63       of int64 (** int for 64 bit arch *)
  | Int32       of int32
  | Int64       of int64
  | Nativeint32 of int32 (** natint for 32 bit arch *)
  | Nativeint64 of int64 (** natint for 64 bit arch *)
  | Float  of float
  | Char   of char
  | String of string
  | List   of t' list
  | Array  of t' list
  | Variant      of XGen.uident * t' option
                      (* Note: OCaml parser parses  F(a,b) always as
                         Variant (_, [a, b])
                         and not as Variant (_, [Tuple (a, b)])
                         
                         Therefore, Variant (_, [Tuple _]) must be invalid
                      *)
  | Poly_variant of XGen.ident * t' option
  | Record of (XGen.lident * t') XGen.non_empty_list
  | Object of (XGen.lident * t') list
  | Tuple of t' XGen.two_or_more_list
  | Unit
  | IntGen of XGen.int_literal * XGen.literal_modifier (** Generic integer with a postfix character, like 1234x *)
  | Var of longident
  | App of t' * ((bool * XGen.lident) option * t') XGen.non_empty_list
  | Seq of t' XGen.two_or_more_list (** t; t; t; .. *)
[@@deriving typerep, sexp]

(* This does NOT check the validity recursively *)
let is_valid_t' = function
  | Int31 i -> -1073741824 <= i && i <= 1073741823 
  | Seq xs ->
      (* must be normalized *)
      List.for_all (function Seq _ -> false | _ -> true) xs
  | Nativeint32 _ -> Sys.int_size = 31
  | Nativeint64 _ -> Sys.int_size = 63
  | Bool _
  | Int63 _
  | Int32 _
  | Int64 _
  | Float  _
  | Char   _
  | String _
  | List   _
  | Tuple  _
  | Array  _ 
  | Variant _
  | Poly_variant _
  | Record _
  | Object _
  | Unit
  | IntGen _
  | Var _ -> true
  | App ( (Unit | Variant _ | Poly_variant _), _) -> false
  | App ((Int32 _ | Int64 _ | Int31 _ | Int63 _ | Float _ | Nativeint32 _ | Nativeint64 _), _) -> false
  | App ((Object _ | Record _ | Bool _ | List _), _) -> false
  | App _ -> true

let () = 
  Sys.catch_break true;
  
  let rec override ((module M : QT.Gen.S) as m) = 
    XGen.override m;
    M.register typerep_of_longident (fun _ex sz ->
      let open Q.Gen in
      let rec mlongident sz = 
        if sz <= 0 then map (fun x -> Lident x) XGen.uident
        else 
          let sz = sz - 1 in
          oneof [ map (fun x -> Lident x) XGen.uident
                ; (mlongident sz >>= fun m ->
                   XGen.uident >>= fun n ->
                   return (Ldot (m, n)))
                ]
      in
      if sz <= 0 then map (fun x -> Lident x) XGen.lident
      else 
        let sz = sz - 1 in
        oneof [ map (fun x -> Lident x) XGen.lident
              ; mlongident sz >>= fun m ->
                XGen.lident >>= fun n ->
                return (Ldot (m, n))
              ]);

    M.register typerep_of_t' (fun ex sz rs ->
        XGen.filter is_valid_t' (gen ~exclude:ex sz) rs)

  and gen ?exclude = QT.Gen.of_typerep 
      ~override
      typerep_of_t ?exclude

  and gen' ?exclude = QT.Gen.of_typerep 
      ~override
      typerep_of_t' ?exclude
  in

  let rec shrink x = QT.Shrink.of_typerep typerep_of_t' ~override x
  and shrink_t x = QT.Shrink.of_typerep typerep_of_t ~override x

  and override ((module M : QT.Shrink.S) as _m) =
    M.register typerep_of_t' (fun x ->
        Q.Iter.filter is_valid_t' @@ shrink_t x);
    M.register1 (module struct
      type 'a t = 'a XGen.non_empty_list
      let typerep_of_t = XGen.typerep_of_non_empty_list
      let typename_of_t = XGen.typename_of_non_empty_list
      let compute : 'a Q.Shrink.t -> 'a XGen.non_empty_list Q.Shrink.t = fun sh -> function 
        | [] -> Q.Shrink.nil []
        | x -> Q.Iter.filter (function [] -> false | _ -> true) @@ Q.Shrink.list ~shrink:sh x
    end);
    M.register1 (module struct
      type 'a t = 'a XGen.two_or_more_list
      let typerep_of_t = XGen.typerep_of_two_or_more_list
      let typename_of_t = XGen.typename_of_two_or_more_list
      let compute : 'a Q.Shrink.t -> 'a XGen.two_or_more_list Q.Shrink.t = fun sh -> function 
        | [] -> Q.Shrink.nil []
        | x -> Q.Iter.filter (function [] | [_] -> false | _ -> true) @@ Q.Shrink.list ~shrink:sh x
    end);
    M.register XGen.typerep_of_lident Q.Shrink.nil;
    M.register XGen.typerep_of_uident Q.Shrink.nil
  in
  
  let print t = Format.asprintf "%a@." Ocaml.format t in

  let arb = Q.make ~shrink (gen' ?exclude:None 5) ~print in

  let test = Q.Test.make arb ~count:1000000 @@ fun t ->
      let s = Format.asprintf "%a" Ocaml.format t in
      let ts = Ocaml.Parser.from_string s in
      match ts with
      | [t'] when t = t' -> true
      | [t'] ->
          Format.eprintf "Test failed for %s@." s;
          Format.eprintf "Parsed back: %a@." Ocaml.format t';
          Format.eprintf "ORIG:   %a@." Sexplib.Sexp.pp_hum @@ sexp_of_t t;
          Format.eprintf "PARSED: %a@." Sexplib.Sexp.pp_hum @@ sexp_of_t t';
          false
      | _ ->
          Format.eprintf "Test failed for %s@." s;
          Format.eprintf "NON SINGLETON ?";
          false
  in
  Q.Test.check_exn test
