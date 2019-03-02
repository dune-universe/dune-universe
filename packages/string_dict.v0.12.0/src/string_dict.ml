open Base
open Ppx_compare_lib.Builtin

(* In order to be efficient, we make assumption about the runtime representation of
   strings. Essentially we assume that it's OK to see a string as a flat array of words
   and that padding bytes are never random.

   Since this is probably not portable in Javascript, the primitive that make these
   assumptions are written in C, where the value representation is documented. Moreover,
   the OCaml compiler is not yet able to generate code that is as efficient as C for the
   [find] function.

   4.04 introduces [Sys.backend_type], so if we can get the OCaml implementation as
   efficient, we can switch back to an OCaml implementation.
*)

(* A block. This module only assume that we can split a string into a list of blocks. *)
type block = nativeint [@@deriving compare]

(* (compact) array of blocks *)
type blocks

(* These functions are used to compile a association list into a trie *)
external blocks_of_string : string -> blocks = "Base_string_dict_blocks_of_string"
external get_block : blocks -> int -> block = "Base_string_dict_get_block"
external num_blocks : blocks -> int = "Base_string_dict_num_blocks"
external make_blocks : block array -> blocks = "Base_string_dict_make_blocks"

(* A dictionary is organized as a trie.  This type is accessed by the C implementation of
   [find]. *)
type 'a trie =
  { num_children : int
  ; (* Block array of length [num_children] *)
    keys         : blocks
  ; (* Array of length [num_children]. [children.(i)] correspond to all the children whose
       nth block is [keys.(i)]. *)
    children     : 'a trie array
  ; (* If this node correspond to an entry, this is the associated value. *)
    value        : 'a option
  }

type 'a t =
  { trie  : 'a trie
  ; (* Sorted association list, used for sexp conversion, comparison and hashing *)
    alist : (string * 'a) list
  }

let to_alist t = t.alist

(* This is the only function for which we really care about performance *)
external find : 'a trie -> string -> 'a option = "Base_string_dict_find" [@@noalloc]

let find t key = find t.trie key

let find_exn t key =
  match find t key with
  | None -> raise Caml.Not_found
  | Some x -> x

module Bmap = Caml.Map.Make(struct
    type t = block [@@deriving compare]
  end)

let rec check_no_duplicates_in_sorted_list = function
  | (a, _) :: ((b, _) :: _ as rest) ->
    if String.compare a b = 0 then
      Error a
    else
      check_no_duplicates_in_sorted_list rest
  | _ -> Ok ()

let sort_and_check_no_duplicates l =
  let l = List.sort l ~compare:(fun (a, _) (b, _) -> String.compare a b) in
  match check_no_duplicates_in_sorted_list l with
  | Ok () -> Ok l
  | Error _ as err -> err

let of_alist l =
  match sort_and_check_no_duplicates l with
  | Error _ as err -> err
  | Ok alist ->
    let rec loop l ~pos =
      let value, l =
        match List.partition_tf l ~f:(fun (blocks, _) -> num_blocks blocks = pos) with
        | [], l -> None, l
        | [(_, x)], l -> Some x, l
        | _ ->
          (* The only way to get here is if we have two entries with the same key, which
             we already checked *)
          assert false
      in
      let keys, subs =
        List.fold l ~init:Bmap.empty ~f:(fun acc ((blocks, _) as entry) ->
          let block = get_block blocks pos in
          let others =
            match Bmap.find block acc with
            | exception (Not_found_s _ | Caml.Not_found) -> []
            | l -> l
          in
          Bmap.add block (entry :: others) acc)
        |> Bmap.bindings
        |> List.unzip
      in
      let keys = make_blocks (Array.of_list keys) in
      let children = Array.of_list (List.map subs ~f:(loop ~pos:(pos + 1))) in
      { num_children = Array.length children
      ; keys
      ; children
      ; value
      }
    in
    let trie =
      loop (List.map alist ~f:(fun (s, x) -> (blocks_of_string s, x))) ~pos:0
    in
    Ok { trie; alist }

let of_alist_exn l =
  match of_alist l with
  | Ok t -> t
  | Error dup ->
    Printf.ksprintf invalid_arg "Dict.make_exn: duplicate key: %S"
      dup

module For_conv = struct
  open Hash.Builtin

  type 'a t = (string * 'a) list [@@deriving compare, hash]

  let sexp_of_t f l =
    Sexp.List (List.map l ~f:(fun (k, v) ->
      Sexp.List [Atom k; f v]))

  let of_sexp_error msg sexp = raise (Sexp.Of_sexp_error (Failure msg, sexp))

  let string_of_sexp : Sexp.t -> string = function
    | Atom s -> s
    | sexp   -> of_sexp_error "atom expected" sexp

  let t_of_sexp f (sexp : Sexp.t) =
    match sexp with
    | Atom _ -> of_sexp_error "list expected" sexp
    | List l ->
      List.map l ~f:(function
        | List [k; v] -> (string_of_sexp k, f v)
        | sexp -> of_sexp_error "s-expression of the form (_ _) expected" sexp)
end

let compare     f a b = For_conv.compare     f a.alist b.alist
let hash_fold_t f s t = For_conv.hash_fold_t f s t.alist
let sexp_of_t   f t   = For_conv.sexp_of_t   f t.alist

let t_of_sexp f sexp =
  let l = For_conv.t_of_sexp f sexp in
  match of_alist l with
  | Ok t -> t
  | Error dup ->
    let msg = Printf.sprintf "Dict.t_of_sexp: duplicated key: %S" dup in
    For_conv.of_sexp_error msg sexp
