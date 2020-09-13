type t = {
  run :
    ?extra_env:(string * string) list ->
    Bos.Cmd.t ->
    (unit, [ `Unknown ]) result;
  run_out : Bos.Cmd.t -> (string, [ `Unknown ]) result;
}

val real : t

val run :
  t ->
  ?extra_env:(string * string) list ->
  Bos.Cmd.t ->
  (unit, [ `Unknown ]) result

val run_out : t -> Bos.Cmd.t -> (string, [ `Unknown ]) result
