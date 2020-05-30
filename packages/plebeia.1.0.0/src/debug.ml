(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Node
open Cursor
open Result
open Utils.Open

let simple_string_of_node : node -> string = fun node ->
  (* pretty prints a tree to a string *)
  let string_of_node n = 
    match Node.index n with
    | None -> "no index"
    | Some i -> string_of_int @@ Index.to_int i
  in
  match node with
  | (Disk (index, _)) -> Printf.sprintf "Disk %Ld" (Index.to_int64 index)
  | View (Leaf (value, Indexed i, Hashed h)) ->
      Printf.sprintf "Leaf %S (%Ld, %s)\n" (Value.to_string value)
        (Index.to_int64 i) (Hex.show @@ Hash.to_hex h)
  | View (Leaf (value, _, _)) ->
      Printf.sprintf "Leaf %S\n" (Value.to_string value)
  | View (Bud  (node , _, _)) ->
    let recursive =
      match node with
      | Some node -> string_of_node node
      | None     ->  "Empty"
    in
    Printf.sprintf "Bud: %s" recursive
  | View (Internal (left, right, _, _)) ->
    Printf.sprintf "Internal: %s%s"
      (string_of_node left)
      (string_of_node right)
  | View (Extender (segment, node, _, _)) ->
    Printf.sprintf "[%s]- %s" (Segment.to_string segment)
      (string_of_node node)
  
let rec string_of_node : node -> int -> string = fun node indent ->
  (* pretty prints a tree to a string *)
  let indent_string = String.concat "" (List.init indent (fun _ -> " . ")) in
  match node with
  | (Disk (index, _)) -> Printf.sprintf "%sDisk %Ld" indent_string (Index.to_int64 index)
  | View (Leaf (value, Indexed i, Hashed h)) ->
      Printf.sprintf "%sLeaf %S (%Ld, %s)\n" indent_string (Value.to_string value)
        (Index.to_int64 i) (Hex.show @@ Hash.to_hex h)
  | View (Leaf (value, _, _)) ->
      Printf.sprintf "%sLeaf %S\n" indent_string (Value.to_string value)
  | View (Bud  (node , _, _)) ->
    let recursive =
      match node with
      | Some node -> string_of_node node (indent + 1)
      | None     ->  "Empty"
    in
    Printf.sprintf "%sBud:\n%s" indent_string recursive
  | View (Internal (left, right, _, _)) ->
    Printf.sprintf "%sInternal:\n%s%s" indent_string
      (string_of_node left (indent + 1))
      (string_of_node right (indent + 1))
  | View (Extender (segment, node, _, _)) ->
    Printf.sprintf "%s[%s]- %s" indent_string (Segment.to_string segment)
      (string_of_node node (indent + 1))

module Dot = struct
  (* Graphviz's dot file format *)
  
  let link ?label n1 n2 =
    match label with
    | None -> Printf.sprintf "%s -> %s;" n1 n2
    | Some l -> Printf.sprintf "%s -> %s [label=\"%s\"];" n1 n2 l

  let linkX ?label n1 n2 =
    match label with
    | None -> Printf.sprintf "%s -> %s [color=red];" n1 n2
    | Some l -> Printf.sprintf "%s -> %s [label=\"%s\", color=red];" n1 n2 l

  let indexed = function
    | Indexed i -> Int64.to_string @@ Index.to_int64 i
    | Not_Indexed -> "n/i"
      
  let modified = function
    | Modified -> "*"
    | Unmodified (ir, _) -> indexed ir
      
  let disk n = Printf.sprintf "%s [shape=box];" n
  let leaf n value ir = Printf.sprintf "%s [label=%S];" n (Value.to_string value ^ ir)
  let bud n ir = Printf.sprintf "%s [shape=diamond, label=\"%s\"];" n ir
  let internal n ir = Printf.sprintf "%s [shape=circle, label=\"%s\"];" n ir
  let extender = internal

  let of_node_aux cntr root =
    let rec aux : int -> node -> (string * string list * int) = fun cntr -> function
      | Disk (index, _) -> 
          let n = Printf.sprintf "Disk%Ld" (Index.to_int64 index) in
          (n, [disk n], cntr)
      | View (Leaf (value, ir, _)) ->
          let n = Printf.sprintf "Leaf%d\n" cntr in
          (n, [leaf n value (indexed ir)], cntr+1)
      | View (Bud  (Some node , ir, _)) ->
          let n', s, cntr = aux cntr node in
          let n = Printf.sprintf "Bud%d" cntr in
          (n, 
           [bud n (indexed ir);
            link n n'
           ] @ s,
           cntr + 1)
      | View (Bud  (None , ir, _)) ->
          let n = Printf.sprintf "Bud%d" cntr in
          (n, 
           [bud n (indexed ir)], 
           cntr + 1)
      | View (Internal (left, right, ir, _)) ->
          let ln, ls, cntr = aux cntr left in 
          let rn, rs, cntr = aux cntr right in 
          let n = Printf.sprintf "Internal%d" cntr in
          (n,
           [ internal n (indexed ir);
             link n ln ~label:"L";
             link n rn ~label:"R" ]
           @ ls @ rs,
           cntr + 1)
      | View (Extender (segment, node, ir, _)) ->
          let n', s, cntr = aux cntr node in
          let n = Printf.sprintf "Extender%d" cntr in
          (n,
           extender n (indexed ir)
           :: linkX n n' ~label:(Segment.to_string segment)
           :: s,
           cntr + 1)
    in
    aux cntr root
  
  let rec of_trail dst cntr = function
    | Top -> ([], cntr)
    | Left (trail, r, mr) ->
        let n = Printf.sprintf "Internal%d" cntr in
        let cntr = cntr + 1 in
        let r, ss, cntr = of_node_aux cntr r in
        let (ss', cntr) = of_trail n cntr trail in
        ([ internal n (modified mr);
           link n dst ~label:"L";
           link n r ~label:"R" ]
         @ ss @ ss',
         cntr)
    | Right (l, trail, mr) ->
        let n = Printf.sprintf "Internal%d" cntr in
        let cntr = cntr + 1 in
        let l, ss, cntr = of_node_aux cntr l in
        let (ss', cntr) = of_trail n cntr trail in
        ([ internal n (modified mr);
           link n l ~label:"L";
           link n dst ~label:"R" ]
         @ ss @ ss',
         cntr)
    | Budded (trail, mr) ->
        let n = Printf.sprintf "Bud%d" cntr in
        let cntr = cntr + 1 in
        let (ss, cntr) = of_trail n cntr trail in
        ([ bud n (modified mr);
           link n dst ]
         @ ss,
         cntr)
    | Extended (trail, segment, mr) -> 
        let n = Printf.sprintf "Extender%d" cntr in
        let cntr = cntr + 1 in
        let (ss, cntr) = of_trail n cntr trail in
        ([ extender n (modified mr);
           link n dst ~label:(Segment.to_string segment) ]
         @ ss,
         cntr)
    
  let make_digraph ss = "digraph G {\n" ^ String.concat "\n" ss ^ "\n}\n"
  
  let of_node root =
    let (_name, ss, _cntr) = of_node_aux 0 root in
    make_digraph ss
  
  let of_cursor (Cursor (trail, node, _)) =
    let (n, ss, cntr) = of_node_aux 0 node in
    let ss', _ = of_trail n cntr trail in
    let s = Printf.sprintf "cursor [shape=point, label=\"\"]; cursor -> %s [style=bold];" n in
    make_digraph (s :: ss @ ss')
end

let dot_of_node = Dot.of_node
let dot_of_cursor = Dot.of_cursor

let () = Cursor.dot_of_cursor_ref := dot_of_cursor
 
(* Bud -> Leaf and Bud -> Bud are invalid, but not excluded by the GADT *)
let validate_node context (node : node) =
  let rec aux : node -> (view, string) Result.t = 
    fun node ->
      let indexed : view -> bool = function
        | Internal (_, _, Indexed _, _) -> true
        | Bud (_, Indexed _, _) -> true
        | Leaf (_, Indexed _, _) -> true
        | Extender (_, _, Indexed _, _) -> true
        | _ -> false
      in
      let hashed_is_transitive : view -> bool = function
        | Internal (_, _, _, Hashed _) -> true
        | Bud (_, _, Hashed _) -> true
        | Leaf (_, _, Hashed _) -> true
        | Extender (_, _, _, Hashed _) -> true
        | _ -> false
      in
      match node with
      | Disk  (i, wit) -> aux @@ View (load_node context i wit) 
      | View v -> 
          match v with
          | Leaf _ -> Ok v
          | Bud (None, _, _) -> Ok v
          | Bud (Some child, ir, hit) ->
              begin
                aux child >>= function
                | Bud _ -> Error "Bud cannot carry Bud"
                | Leaf _ -> Error "Bud cannot carry Leaf"
                | v' ->
                    (match ir, indexed v' with
                     | _, true -> Ok ()
                     | Not_Indexed, _ -> Ok ()
                     | _ -> Error "Bud: Strange indexed") >>= fun () ->
                    (match hit, hashed_is_transitive v' with
                     | _, true -> Ok v
                     | Not_Hashed, _ -> Ok v
                     | _ -> Error "Bud: Strange hashed_is_transitive")
              end
          | Internal (l, r, ir, hit) ->
              begin
                aux l >>= fun l ->
                aux r >>= fun r -> 
                (match ir, indexed l, indexed r with
                 | _, true, true -> Ok ()
                 | Not_Indexed, _, _ -> Ok ()
                 | Indexed _, _, _ -> Error "Internal: Strange indexed") >>= fun () ->
                (match hit, hashed_is_transitive l, hashed_is_transitive r with
                 | _, true, true -> Ok v
                 | Not_Hashed, _, _ -> Ok v
                 | _ -> Error "Internal: Strange hashed_is_transitive")
              end
          | Extender (seg, node, ir, hit) ->
              aux node >>= function
              | Extender _ -> Error "Extender cannot carry Extender"
              | v' ->
                  if Segment.length seg > Segment.max_length then 
                    Error "segment too long"
                  else 
                    (match ir, indexed v' with
                     | _, true -> Ok ()
                     | Not_Indexed, _ -> Ok ()
                     | _ -> Error "Extender: Strange hashed_is_transitive") >>= fun () ->
                    (match hit, hashed_is_transitive v' with
                     | _, true -> Ok v
                     | Not_Hashed, _ -> Ok v
                     | _ -> Error "Extender: Strange hashed_is_transitive")
  in
  aux node >>= function
  | Bud _ -> Ok ()
  | _ -> Error "Tree must start with a Bud"

let save_cursor_to_dot name c = to_file ~file:name @@ dot_of_cursor c
let save_node_to_dot name n = to_file ~file:name @@ dot_of_node n
