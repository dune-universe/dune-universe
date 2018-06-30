module Error = struct
  type t = AlreadySet
         | NotSquare

  module Exn = struct
    exception AlreadySet
    exception NotSquare
  end

  let to_exn (err : t) : exn =
    match err with
    | AlreadySet -> Exn.AlreadySet
    | NotSquare  -> Exn.NotSquare

  let unwrap (res : ('a, t) result) : 'a =
    match res with
    | Ok x    -> x
    | Error e -> raise (to_exn e)
end

type tree = { mutable matrix : Matrix.t option;
              children       : tree option array;
            }

type t = {  root         : tree;
            total_shards : int;
         }

module Tree : sig
  val get_inverted_matrix : tree -> int list -> int -> int -> Matrix.t option

  val insert_inverted_matrix : tree -> Matrix.t -> int list -> int -> int -> unit
end = struct
  let make (matrix : Matrix.t option) (children_count : int) : tree =
    { matrix;
      children = Array.make children_count None }

  let get_child
      (tree            : tree)
      (offset          : int)
      (requested_index : int)
      (total_shards    : int )
    : tree =
    let node_index = requested_index - offset in

    match tree.children.(node_index) with
    | None -> (
        let child = make None (total_shards - offset) in
        tree.children.(node_index) <- Some child;
        child
      )
    | Some x -> x

  let rec get_inverted_matrix
      (tree            : tree)
      (invalid_indices : int list)
      (total_shards    : int)
      (offset          : int)
    : Matrix.t option =
    match invalid_indices with
    | [] -> tree.matrix
    | requested_index :: remaining_indices -> (
        let child = get_child tree offset requested_index total_shards in
        get_inverted_matrix
          child
          remaining_indices
          total_shards
          (requested_index + 1)
      )

  let rec insert_inverted_matrix
      (tree            : tree)
      (matrix          : Matrix.t)
      (invalid_indices : int list)
      (total_shards    : int)
      (offset          : int)
    : unit =
    match invalid_indices with
    | [] -> tree.matrix <- Some matrix
    | requested_index :: remaining_indices -> (
        let child = get_child tree offset requested_index total_shards in
        insert_inverted_matrix
          child
          matrix
          remaining_indices
          total_shards
          (requested_index + 1)
      )
end

let make (data_shards : int) (parity_shards : int) : t =
  { root         = { matrix   = Some (Matrix.identity data_shards);
                     children = Array.make (data_shards + parity_shards) None; };
    total_shards = data_shards + parity_shards; }

let get_inverted_matrix (tree : t) (invalid_indices : int array) : Matrix.t option =
  match Array.to_list invalid_indices with
  | [] -> tree.root.matrix
  | l  -> Tree.get_inverted_matrix tree.root l tree.total_shards 0

let insert_inverted_matrix
    (tree            : t)
    (invalid_indices : int array)
    (matrix          : Matrix.t)
  : (unit, Error.t) result =
  if not (Matrix.is_square matrix) then
    Error NotSquare
  else (
    match Array.to_list invalid_indices with
    (* If no invalid indices were given then we are done because the
     * root node is already set with the identity matrix. *)
    | [] -> Error AlreadySet
    | l  -> Ok (Tree.insert_inverted_matrix tree.root matrix l tree.total_shards 0)
  )
