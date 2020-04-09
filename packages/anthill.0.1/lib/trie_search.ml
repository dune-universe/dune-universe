open Core
include Trie
include Types

(*************************************************************************
 * search -> trie -> wordlist
 * ***********************************************************************)

let space = 32 - 97

let upper i = i + 65 - 97

let char_of_int i = match Char.of_int (i + 97) with
  | Some c -> c
  | None -> '#'

(*
let display sw =
  match sw with
  | None -> "[]"
  | Some w -> w
   *)

let word_of prefix =
  Some (String.of_char_list (List.map (List.rev prefix) ~f:char_of_int))

(*************************************************************************
 * search functions starting from an arbitrary node + prefix
 * ***********************************************************************)

let collecting traversal =
  let retval = ref [] in
  let add_word = function None -> () | Some s -> retval := s :: !retval in
  traversal add_word;
  !retval

(* follow a 'trail' of characters or wildcards starting from a given prefix *)
let _pattern trie prefix trail =
  let traversal add_word =
    let rec traverse node prefix trail =
      match trail with
      | [] -> ();
      | c :: cs -> begin
        let next_step c child = follow child cs (c :: prefix) in
        match c with
        | Dot -> Trie.foreach_child node next_step
        | Star -> begin
          follow node cs prefix;
          Trie.foreach_child node next_step;
          Trie.foreach_child node (fun c child -> follow child trail (c :: prefix))
          end
        | Letter i -> Trie.with_child node i next_step
        | Group g -> Trie.for_child_in_group node g next_step
      end
    and follow node cs prefix =
      if (List.is_empty cs && node.eow) then add_word (word_of prefix);
      traverse node prefix cs
    in
      traverse trie prefix trail
  in collecting traversal |> Wordset.of_list
;;

(* find all possibilities for a single wildcard *)
let _fit trie prefix trail =
  let traversal add_word =
    let rec traverse node prefix trail =
      match trail with
      | [] -> ();
      | c :: cs -> begin
        let next_step add c child = match add with
          | true -> follow child cs (c :: prefix)
          | false -> follow child cs prefix
        in
        match c with
        | Dot -> Trie.foreach_child node (next_step true)
        | Letter i -> Trie.with_child node i (next_step false)
        | _ -> raise Unsupported_feature
      end
    and follow node cs prefix =
      if (List.is_empty cs && node.eow) then add_word (word_of prefix);
      traverse node prefix cs
    in
      traverse trie prefix trail
  in
  let ws = collecting traversal in
  let ws = List.filter ws ~f:(fun s -> String.length s > 0) in
  List.map ws ~f:(fun s -> s.[0])
;;

(* Build all possible words from a bag and a trie *
 * if all = false, return only words using the entire bag *)
let _anagram trie prefix trail ~all ~multi =
  let bag = Mutable_rack.of_rack trail in
  let traversal add_word =
    let rec traverse node prefix =
      Trie.foreach_child node (fun c child ->
          follow child c prefix)
    and follow node c prefix =
      let letter = Mutable_rack.play bag c in
      match letter with
      | None -> ()
      | Some played ->
        begin
          let prefix = match played with
          | Letter _ -> c :: prefix
          | _ -> (upper c) :: prefix
          in
          if (Mutable_rack.is_empty bag) && node.eow then
            add_word (word_of prefix)
          else
            begin
              if all && node.eow then
                add_word (word_of prefix);
              if multi && node.eow then begin
                traverse trie (space :: prefix);
              end
            end;
          traverse node prefix;
          Mutable_rack.add bag played
        end
    in
    traverse trie prefix;
  in collecting traversal |> Wordset.of_list
;;

let expand_groups trail =
  let is_group = function Group _ -> true | _ -> false in
  let (groups, rest) = List.partition_tf trail ~f:is_group in
  let groups = List.map groups ~f:(function Group i -> i | _ -> []) in
  let expanded_groups = Groupset.product groups in
  List.map expanded_groups ~f:(fun i ->
      List.append (List.map i ~f:(fun j -> Letter j)) rest)

let make_anags trie trail ~multi ~all =
  let trails = expand_groups trail in
  Wordset.union_list (
    List.map trails ~f:(fun trail -> _anagram trie [] trail ~multi ~all))

module TrieEngine = struct
  type dict = Trie.t

  let pattern trie trail = _pattern trie [] trail

  let fit trie trail = _fit trie [] trail

  let anagram trie trail ~multi ~all =
    make_anags trie trail ~multi ~all

  let exists _ _ = true
end
