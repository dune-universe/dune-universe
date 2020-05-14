open Camlon

let () =
  match Ocaml.load "data.caml" with
  | Error e ->
      Format.eprintf "data.caml: %a@." Ocaml.Parser.format_error e;
      exit 2
  | Ok vs ->
      Ocaml.save ~perm:0o644 "data.caml.out" vs;
      match Ocaml.load "data.caml.out" with
      | Error e ->
          Format.eprintf "data.caml.out: %a@." Ocaml.Parser.format_error e;
          exit 2
      | Ok vs' ->
          if vs = vs' then prerr_endline "Ok."
          else begin
            prerr_endline "load => save => load is not idempotent!";
            exit 2
          end
