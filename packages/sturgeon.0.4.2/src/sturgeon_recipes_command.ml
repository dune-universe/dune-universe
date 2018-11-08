open Sturgeon

let command ?greetings ?cogreetings () =
  let stdin = Sexp.of_channel stdin in
  let stdout sexp =
    Sexp.tell_sexp print_string sexp;
    print_newline ();
    flush stdout
  in
  let stdin', status = Session.connect ?greetings ?cogreetings stdout in
  let rec aux () =
    match stdin () with
    | None -> exit 0
    | Some sexp ->
      stdin' sexp;
      if Session.pending_continuations status > 0 then
        aux ()
      else exit 0
  in
  aux ()

let text_command f =
  let greetings, shell = Stui.buffer_greetings () in
  command ~greetings ~cogreetings:(fun args -> f ~args shell) ()
