(*
  pre-compiled efficient sprintf $"xxx"
*)

let _ =
  assert ("hello" = {qq|hello|qq});
  let w = "world" in
  assert ("hello world" = {qq|hello %s|qq} w);
  assert ("hello world" = {qq|hello $w|qq});
  assert ("hello worldworld" = {qq|hello ${w ^ w}|qq});

  let x = 42 in
  assert ("answer = 42" = {qq|answer = %d|qq} x);
  assert ("answer = 42" = {qq|answer = %${x}d|qq});
  assert ("answer^2 = 1764" = {qq|answer^2 = %${x * x}d|qq});
  (* assert ("123." = {qq|%${123.456}1.F|qq}); (* it fails if ocaml bug in printf.ml is not fixed *) *)
;;

