let set_output =
  let output =
    let open Js_of_ocaml in
    let open Dom_html in
    let el = window##.document##getElementById (Js.string "output") in
    Js.coerce_opt el CoerceTo.blockquote (fun _el -> Format.eprintf "error: can't find blockquote with id `output`"; assert false)
  in
  let pp_row fmt row =
    Flex_array.pp ~pp_sep:(fun fmt () -> Format.fprintf fmt "|") (fun fmt cel -> Format.fprintf fmt "%s" cel) fmt row
  in
  let pp fmt table =
    Flex_array.pp ~pp_sep:(fun fmt () -> Format.fprintf fmt "<br />@.") pp_row fmt table
  in
  fun content ->
    let content = Format.asprintf "%a" pp content in
    output##.innerHTML := Js_of_ocaml.Js.string @@ content

module M = Atable.Make (struct
  let initial = None
  let table_id = "table"
  let add_row_button_id = Some "addrow"
  let add_col_button_id = Some "addcol"
  let rm_row_button_id = Some "rmrow"
  let rm_col_button_id = Some "rmcol"
  let reset_button_id = Some "reset"
  let action_button_id = Some "eval"
  let action_fun = Some set_output
end)

let () =
  M.init ();
  ()
