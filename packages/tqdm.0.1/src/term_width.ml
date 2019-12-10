external term_width : unit -> int = "ocaml_term_width"

let get () =
  let term_width = term_width () in
  if term_width <= 0 then None else Some term_width
