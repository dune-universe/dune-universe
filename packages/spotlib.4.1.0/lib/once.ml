type 'a t = 'a option ref

let create () = ref None

exception Already_initialized

let set x v = match x with
  | {contents = Some _} -> raise Already_initialized
  | r -> r := Some v

let get x = !x

