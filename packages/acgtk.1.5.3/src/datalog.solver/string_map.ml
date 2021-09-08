module String_map = Map.Make(
  struct
    type t=string
    let compare =Stdlib.compare
  end
)
