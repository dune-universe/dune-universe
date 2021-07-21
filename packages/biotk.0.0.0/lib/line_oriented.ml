open Core
open Biocaml_base

let lines () =
  let open Lines.Parser in
  Biotk_pipes_unix.Pipe.loop step initial_state

module type Item = sig
  type t
  val parse : Line.t -> t
  val unparse : t -> string
end

module type S = sig
  type item
  val load : string -> item list
  val fold : string -> init:'a -> f:('a -> item -> 'a) -> 'a

  val save : item list -> string -> unit
end

module Make(Item : Item) = struct
  type item = Item.t

  let load fn =
    In_channel.read_lines fn
    |> List.map ~f:(fun l ->
        Item.parse (Line.of_string_unsafe l)
      )

  let fold fn ~init ~f =
    let open Biotk_pipes_unix.Pipe in
    run (
      from_file fn
      $$ lines ()
      $$ map Item.parse
      $$ fold init (Fn.flip f)
    )

  let save items fn =
    let open Out_channel in
    with_file fn ~f:(fun oc ->
        List.iter items ~f:(fun item ->
            output_string oc (Item.unparse item) ;
            output_char oc '\n'
          )
      )
end
