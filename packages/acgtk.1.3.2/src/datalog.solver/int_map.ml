module Int_map = Map.Make(
  struct
    type t=int
    let compare = Pervasives.compare
  end
)
