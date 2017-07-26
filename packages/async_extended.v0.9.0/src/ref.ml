open Async

type 'a t = {
  mutable value : 'a;
  tail : 'a Tail.t;
}

let create value = {
  value = value;
  tail = Tail.create ();
}

let get t = t.value

let set t v =
  t.value <- v;
  Tail.extend t.tail v

let listen_to_sets t = Tail.collect t.tail

let register t ?(init = ignore) ~f =
  init (get t);
  ignore
    (Stream.iter' (listen_to_sets t) ~f:(fun a ->
      match f a with
      | `Continue -> Deferred.unit
      | `Leave -> Deferred.never ()))
