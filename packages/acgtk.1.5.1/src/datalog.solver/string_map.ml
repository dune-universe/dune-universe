module String_map = Map.Make(
  struct
    type t=string
    let compare =Pervasives.compare
  end
)
