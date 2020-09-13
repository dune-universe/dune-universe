open! Import

type t = {
  run :
    ?extra_env:(string * string) list ->
    Bos.Cmd.t ->
    (unit, [ `Unknown ]) result;
  run_out : Bos.Cmd.t -> (string, [ `Unknown ]) result;
}

module Real = struct
  let get_env () =
    Bos.OS.Env.current () |> Rresult.R.reword_error (fun _ -> `Unknown)

  let run ?extra_env cmd =
    let open Let_syntax.Result in
    let* env =
      match extra_env with
      | None -> Ok None
      | Some l ->
          let+ cur = get_env () in
          let seq = List.to_seq l in
          Some (Astring.String.Map.add_seq seq cur)
    in
    Bos.OS.Cmd.in_null |> Bos.OS.Cmd.run_in ?env cmd
    |> Rresult.R.reword_error (fun _ -> `Unknown)

  let run_out cmd =
    Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string
    |> Rresult.R.reword_error (fun _ -> `Unknown)
end

let real =
  let open Real in
  { run; run_out }

let run_out t = t.run_out

let run t = t.run
