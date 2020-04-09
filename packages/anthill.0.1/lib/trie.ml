open Core

(* trie node *)
type node = {
  mutable eow: bool;
  mutable children: ((node option) array) option;
}

(* provide access as Trie.t when passing a trie around *)
type t = node

let new_node eow = {
  eow = eow;
  children = None;
}

let new_child_array () = Array.create ~len:26 None

let is_none n = phys_equal n None

(* letter <-> index *)
let index c = (Char.to_int c) - 97
let letter i = match Char.of_int (i + 97) with
  | Some c -> c
  | None -> '#'

let add node letters =
  let n = ref node in
  let add_letter letter =
    let ix = index letter in
    if is_none !n.children then
      !n.children <- Some (new_child_array ());
    match !n.children with
    | None -> (); (* should never happen *)
    | Some ch -> begin
        if is_none ch.(ix) then
          ch.(ix) <- Some (new_node false);
        match ch.(ix) with
        | None -> ()
        | Some node -> n := node
    end
    in
    String.iter letters ~f:add_letter;
    !n.eow <- true

let foreach_child node f =
  match node.children with
  | None -> ()
  | Some ch ->
    Array.iteri ch ~f:(fun i c ->
        match c with
        | None -> ()
        | Some child -> f i child
      )

let for_child_in_group node group f =
  match node.children with
  | None -> ()
  | Some ch ->
      List.iter group ~f:(fun i ->
        match ch.(i) with
        | None -> ()
        | Some child -> f i child
      )

let with_child node i f =
  match node.children with
  | None -> ()
  | Some ch ->
      match ch.(i) with
      | None -> ()
      | Some child -> f i child


let char_of_int i = match Char.of_int (i + 97) with
  | Some c -> c
  | None -> '#'

let word_of prefix =
  String.of_char_list (List.map (List.rev prefix) ~f:char_of_int)

let printall node prefix =
  let rec traverse node prefix =
    if node.eow then
      Printf.printf "%s\n" (word_of prefix);
    foreach_child node (fun c child ->
      traverse child (c :: prefix))
  in
  traverse node prefix

let of_list words =
  let root = new_node false in
  List.iter words ~f:(fun w -> add root w);
  root

let load_from_text_file filename =
  let words = In_channel.read_lines filename in
  of_list words
