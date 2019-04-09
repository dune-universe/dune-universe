(** SCGI request headers *)
type t = (string * string) list

let null = Char.chr 0

let of_string s =
  let rec loop lst index =
    try
      let next = String.index_from s index null in
      let name = String.sub s index (next - index) in
      let index = next + 1 in
      let next = String.index_from s index null in
      let value = String.sub s index (next - index) in
      loop ((name, value) :: lst) (next + 1)
    with
    | Not_found ->
        List.rev lst
  in
  loop [] 0
