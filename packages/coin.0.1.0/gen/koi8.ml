type code = int
type name = string
type map  = code * Uchar.t * name

module Binding =
struct
  open Angstrom

  let wsp = skip_while Astring.Char.Ascii.is_white

  let binding =
    take_while ((<>) ':') >>= fun name -> char ':' *> wsp *>
    take_while1 (fun _ -> true) >>= fun value ->
    return (Astring.String.trim name, Astring.String.trim value)
end

let extract source =
  let _, maps = List.partition Source.is_comment source in
  List.fold_left (fun map -> function
      | Source.Map { a; b; name; } -> Ptmap.add a (b, (Astring.String.trim name)) map
      | _ -> assert false) Ptmap.empty maps
  |> Rresult.R.ok
