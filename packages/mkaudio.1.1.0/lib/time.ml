let parse_duration str =
  try
    Scanf.sscanf str "%f%c"
      (fun num interval ->
        match interval with
        | 's' -> Result.Ok (num *. 1.0)
        | 'm' -> Result.Ok (num *. 60.0)
        | 'h' -> Result.Ok (num *. 3600.0)
        | _   -> Result.Error (Printf.sprintf "Unknown interval: %c" interval))
  with End_of_file | Scanf.Scan_failure _ ->
    Result.Error (Printf.sprintf "Malformed duration: %s" str)

let calculate_samples ~sample_rate ~duration ~tempo ~steps =
  match duration, tempo, steps with
  | Some duration, None, None ->
    Result.Ok
      ((float_of_int sample_rate) *. duration
      |> int_of_float)
  | None, Some tempo, Some steps ->
    Result.Ok
      ((float_of_int (sample_rate * steps * 60 / 4)) /. tempo
      |> int_of_float)
  | None, None, None
  | Some _, _, _ ->
    Result.Error
      "You either need to specify the duration or the tempo and number of beats"
  | None, _, _ ->
    Result.Error "You need to specify both the tempo and number of beats"
