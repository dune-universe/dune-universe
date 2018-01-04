open Core_kernel
open Ppx_core.Light
open Expect_test_common.Std
open Expect_test_matcher.Std

let declare_extension name ~is_exact =
  Extension.Expert.declare name
    Extension.Context.structure_item
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~is_exact)

let expect       = declare_extension "expect"       ~is_exact:false
let expect_exact = declare_extension "expect_exact" ~is_exact:true

let expect_extensions = [expect; expect_exact]

let part_attr =
  Attribute.Floating.declare "toplevel_expect_test.part"
    Attribute.Floating.Context.structure_item
    Ast_pattern.(single_expr_payload (estring __))
    (fun s -> s)

type chunk =
  { part        : string option
  ; phrases     : toplevel_phrase list
  ; expectation : Fmt.t Cst.t Expectation.t
  ; phrases_loc : Location.t
  }

let split_chunks ~fname phrases =
  let rec loop ~loc_start ~part phrases code_acc acc =
    match phrases with
    | [] ->
      if code_acc = [] then
        (List.rev acc, None)
      else
        (List.rev acc, Some (List.rev code_acc, loc_start, part))
    | phrase :: phrases ->
      match phrase with
      | Ptop_def [] -> loop phrases code_acc acc ~loc_start ~part
      | Ptop_def [{pstr_desc = Pstr_extension(ext, attrs); pstr_loc = loc}] -> begin
          match Extension.Expert.convert expect_extensions ext ~loc with
          | None -> loop phrases (phrase :: code_acc) acc ~loc_start ~part
          | Some f ->
            assert_no_attributes attrs;
            let e =
              { phrases     = List.rev code_acc
              ; expectation = Expectation.map_pretty (f ~extension_id_loc:(fst ext).loc)
                                ~f:Lexer.parse_pretty
              ; phrases_loc =
                  { loc_start
                  ; loc_end   = loc.loc_start

                  ; loc_ghost = false
                  }
              ; part
              }
            in
            loop phrases [] (e :: acc) ~loc_start:loc.loc_end ~part
        end
      | Ptop_def [{pstr_desc = Pstr_attribute _; pstr_loc = loc} as item] -> begin
          match Attribute.Floating.convert [part_attr] item with
          | None -> loop phrases (phrase :: code_acc) acc ~loc_start ~part
          | Some part ->
            match code_acc with
            | _ :: _ ->
              Location.raise_errorf ~loc
                "[@@@part ...] cannot appear in the middle of a code block."
            | [] ->
              loop phrases [] acc ~loc_start:loc.loc_end ~part:(Some part)
        end
      | _ -> loop phrases (phrase :: code_acc) acc ~loc_start ~part
  in
  loop phrases [] [] ~part:None
    ~loc_start:{ Lexing.
                 pos_fname = fname
               ; pos_bol   = 0
               ; pos_cnum  = 0
               ; pos_lnum  = 1
               }
;;

(** Extract the subset of the contents of a string, based on an OCaml AST location. *)
let extract_by_loc contents (loc : Location.t) =
  let start = loc.loc_start.pos_cnum in
  let stop  = loc.loc_end.pos_cnum   in
  String.sub contents ~pos:start ~len:(stop - start)
;;

let render_expect : _ Cst.t Expectation.Body.t -> string = function
  | Exact s -> s
  | Pretty cst -> Cst.to_string cst |> String.strip
;;

let declare_org_extension name =
  Extension.Expert.declare name
    Extension.Context.expression
    Ast_pattern.(
      map (single_expr_payload (pexp_loc __ (pexp_constant (pconst_string __ __))))
        ~f:(fun f loc s tag -> f (Some (loc, s, tag)))
      |||
      map (pstr nil)
        ~f:(fun f -> f None)
    )
    (fun payload -> match payload with
       | None -> ""
       | Some (_, s, _) -> s)

let org = declare_org_extension "org"
let org_extensions = [org]

type mlt_block =
  | Org of string
  | Expect of string
  | Code of string
[@@deriving sexp]

let parse phrases ~contents =
  let rec loop phrases acc =
    match phrases with
    | [] -> List.rev acc (* Base case: no more phrases to parse. *)
    | phrase :: phrases ->
      match phrase with
      | Ptop_def [{pstr_desc = Pstr_extension(ext, attrs); pstr_loc = loc}] -> begin
          match (Extension.Expert.convert org_extensions ext ~loc,
                 Extension.Expert.convert expect_extensions ext ~loc) with
          | (Some body, None) ->
            loop phrases (Org body :: acc)
          | (None, Some f) ->
            assert_no_attributes attrs;
            let expectation = Expectation.map_pretty (f ~extension_id_loc:(fst ext).loc)
                                ~f:Lexer.parse_pretty
            in
            let body = render_expect expectation.body in
            loop phrases (Expect body :: acc)
          | _ -> loop phrases acc (* It doesn't really make sense for it to be BOTH an
                                     [org] and an [expect] extension, so we ignore this
                                     case. *)
        end
      | Ptop_def [{pstr_desc = Pstr_attribute _; pstr_loc = _} as item] -> begin
          match Attribute.Floating.convert [part_attr] item with
          | None -> loop phrases acc
          | Some _ -> loop phrases acc (* Discard [@@@part] declarations. *)
        end
      | Ptop_def [{pstr_desc = _; pstr_loc = loc}] -> begin
          let body = extract_by_loc contents loc in
          loop phrases (Code body :: acc)
        end
      | _ -> loop phrases acc
  in
  loop phrases []
;;
