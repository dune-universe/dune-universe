module CamlLexer = Lexer
open Core_kernel
open Ppxlib
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

let split_chunks ~fname ~allow_output_patterns phrases =
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
                                ~f:(Lexer.parse_pretty ~allow_output_patterns)
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

module Chunks = struct
  (* Comments are discarded by the parser that passes phrases to this function, so we must
     expand the locations to include top-level comments. *)

  type position = Lexing.position
  let sexp_of_position { Lexing.pos_cnum; _} = [%sexp (pos_cnum : int)]

  type location = Location.t =
    { loc_start : position
    ; loc_end   : position
    ; loc_ghost : bool
    }
  [@@deriving sexp_of]

  module Chunk = struct
    type 'a t =
      | Expansive of location
      | Fixed of { loc : location
                 ; value : 'a
                 }
      | Ignored of location
    [@@deriving sexp_of]

    let loc = function
      | Expansive loc
      | Fixed     { loc; value = _ }
      | Ignored   loc
        -> loc
    ;;

  end

  type 'a t = 'a Chunk.t Queue.t
  [@@deriving sexp_of]

  let expansive (t : _ t) loc       = Queue.enqueue t (Expansive loc)
  let fixed     (t : _ t) loc value = Queue.enqueue t (Fixed     { loc; value })
  let ignored   (t : _ t) loc       = Queue.enqueue t (Ignored   loc)

  let make_empty_loc ~pos_cnum : Location.t =
    let pos : position = { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum } in
    { loc_start = pos; loc_end = pos; loc_ghost = false }
  ;;

  let create () = Queue.create ()

  let locs_without_gaps t ~final_pos_cnum =
    let nonempty_loc loc_start loc_end : Location.t =
      assert (Int.(<) loc_start.Lexing.pos_cnum loc_end.Lexing.pos_cnum);
      { loc_start; loc_end; loc_ghost = false }
    in
    let make_filler ~(prev : Location.t) ~(next : Location.t) : 'a Chunk.t option =
      let cmp = [%compare: int] prev.loc_end.pos_cnum next.loc_start.pos_cnum in
      match Ordering.of_int cmp with
      | Less ->
        Some (Expansive (nonempty_loc prev.loc_end next.loc_start))
      | Equal -> None
      | Greater -> raise_s [%message "Overlap." (prev : location) (next : location)]
    in
    let rec fill_gaps final_loc chunks acc =
      match chunks with
      | [] -> acc
      | [ chunk ] ->
        make_filler ~prev:(Chunk.loc chunk) ~next:final_loc
        |> Option.fold ~init:(chunk :: acc) ~f:(Fn.flip List.cons)
      | car :: (cadr :: _ as cdr) ->
        make_filler ~prev:(Chunk.loc car) ~next:(Chunk.loc cadr)
        |> Option.fold ~init:(car :: acc) ~f:(Fn.flip List.cons)
        |> fill_gaps final_loc cdr
    in
    let rec merge_expansive_chunks acc chunks : 'a Chunk.t list =
      match chunks with
      | [] -> acc
      | [ chunk ] -> chunk :: acc
      | car :: (cadr :: cddr as cdr) ->
        match car, cadr with
        | (Fixed _ | Ignored _), _ | _, (Fixed _ | Ignored _) ->
          merge_expansive_chunks (car :: acc) cdr
        | Expansive prev, Expansive next ->
          let loc =
            (* Flipped because [merge_expansive_chunks] sees the chunks backwards *)
            assert (Int.(=) next.loc_end.pos_cnum prev.loc_start.pos_cnum);
            nonempty_loc next.loc_start prev.loc_end
          in
          merge_expansive_chunks acc (Expansive loc :: cddr)
    in
    merge_expansive_chunks []
      (fill_gaps (make_empty_loc ~pos_cnum:final_pos_cnum) (Queue.to_list t) [])
  ;;

end

let parse phrases ~contents =
  let chunks = Chunks.create () in
  List.iter phrases ~f:(function
    | Ptop_def structure_items ->
      List.iter structure_items ~f:(fun ({ pstr_desc; pstr_loc = loc } as item) ->
        match pstr_desc with
        | Pstr_extension (ext, attrs) -> begin
            match (Extension.Expert.convert org_extensions ext ~loc,
                   Extension.Expert.convert expect_extensions ext ~loc) with
            | (Some body, None) ->
              Chunks.fixed chunks loc (`Org body);
            | (None, Some f) ->
              assert_no_attributes attrs;
              let expectation = Expectation.map_pretty (f ~extension_id_loc:(fst ext).loc)
                                  ~f:(Lexer.parse_pretty ~allow_output_patterns:false)
              in
              let body = render_expect expectation.body in
              Chunks.fixed chunks loc (`Expect body)
            | None, None -> ()
            | Some _, Some _ ->
              let s = extract_by_loc contents loc in
              raise_s [%message "Both an org and an expect node." s]
          end
        | Pstr_attribute _ -> begin
            match Attribute.Floating.convert [part_attr] item with
            (* Documentation comments can desugar into a top-level [doc] attribute. *)
            | None -> Chunks.expansive chunks loc
            | Some _ -> Chunks.ignored chunks loc (* Discard [@@@part] declarations. *)
          end
        | _ -> Chunks.expansive chunks loc)
    | Ptop_dir _ -> ());
  Chunks.locs_without_gaps chunks ~final_pos_cnum:(String.length contents)
  |> List.filter_map ~f:(function
    | Fixed { loc = _; value = `Org    body } -> Some (Org    body)
    | Fixed { loc = _; value = `Expect body } -> Some (Expect body)
    | Ignored _                               -> None
    | Expansive loc ->
      let code = extract_by_loc contents loc in
      if String.is_empty code
      then None
      else Some (Code code))
;;
