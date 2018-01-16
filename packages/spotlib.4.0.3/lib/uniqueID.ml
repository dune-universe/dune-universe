type t = int ref

let create () = ref 0

let get r =
  let v = !r in
  if v < 0 then failwith "UniqueID.get: overflow";
  incr r;
  v
