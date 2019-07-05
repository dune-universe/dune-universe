(* Example adapted from https://github.com/ocaml-ppx/ppx_import/ *)

[%%rewrite
  module%import Longident = struct
    type t = Longident.t [@printer Printtyp.longident]
          [@@rewrite] [@@remove]
    (* Rewriting rules at this point:
       - t -> Longident.t [@printer Printtyp.longident] *)
  end
  (* Rewriting rules at this point:
     - Longident.t -> Longident.t [@printer Printtyp.longident] *)

  module%import Asttypes = struct
    type 'a loc = 'a Asttypes.loc
          [@polyprinter fun pp fmt x -> pp fmt x.Asttypes.txt]
          [@@rewrite] [@@remove]
  end

  module%import Parsetree = struct
    type core_type = Parsetree.core_type [@printer Pprintast.core_type]
          [@@rewrite] [@@remove]

    (* Rewriting rules at this point:
       - Longident.t -> Longident.t [@printer Printtyp.longident]
       - 'a Asttypes.loc -> 'a Asttypes.loc
          [@polyprinter fun pp fmt x -> pp fmt x.Asttypes.txt]
       - core_type -> Parsetree.core_type [@printer Pprintast.core_type] *)

    type package_type = _ [@@deriving show]
  end]

let core_type_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.parse_core_type Lexer.token lexbuf

let test () =
  let ptyp =
    match core_type_of_string {|(module S with type t1 = t2)|} with
    | { ptyp_desc = Ptyp_package ptyp; _ } -> ptyp
    | _ -> assert false in
  assert (show_package_type ptyp = "(S, [(t1, t2)])")

let () = test ()
