open Js_of_ocaml
open Wikitext

let () =
  let getElementById coerce id =
    match Js.Opt.to_option @@ Dom_html.document##getElementById (Js.string id) with
    | None -> failwith id
    | Some x -> match Js.Opt.to_option @@ coerce x with
                | None -> failwith id
                | Some x -> x
  in
  let input = getElementById Dom_html.CoerceTo.textarea "input" in
  let output = getElementById Dom_html.CoerceTo.div "output" in
  let update () =
    try
      let doc = doc_from_string (Js.to_string input##.value) in
      let doc = Mapper.set_toc doc in
      let doc = Mapper.set_links doc in
      output##.innerHTML := Js.string @@ doc_to_string doc
    with
    | ParsingError (line, col, lexeme) ->
      output##.innerHTML :=
        Js.string @@ Printf.sprintf "line %d, col %d, lexeme [%s]" line col lexeme
  in
  input##.oninput := Dom.handler (fun _ -> update () ; Js._false) ;
  update ()
