open! Core_kernel

module Make (Hashable : Hashable.S) = struct
  type t =
    { capacity : int
    ; hash_queue : unit Hashable.Hash_queue.t
    }

  let create ~capacity = { capacity; hash_queue = Hashable.Hash_queue.create () }

  let mem { hash_queue; _ } value =
    match Hash_queue.lookup_and_move_to_back hash_queue value with
    | Some () -> true
    | None -> false
  ;;

  let add ({ hash_queue; capacity } as t) value =
    match mem t value with
    | true -> ()
    | false ->
      Hash_queue.enqueue_back_exn hash_queue value ();
      (match Hash_queue.length hash_queue > capacity with
      | false -> ()
      | true -> Hash_queue.drop hash_queue `front)
  ;;
end
