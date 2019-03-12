
let slashes = Re2.create_exn "/"
let special_characters = Re2.create_exn "[^0-9a-zA-Z_]+"

(* TODO: better specs for this. validate as well (or maybe when route gets
 *  * created. *)
let id_from_template t =
    t |> Re2.replace_exn ~f:(fun _ -> "_") slashes
      |> Re2.replace_exn ~f:(fun _ -> "") special_characters
