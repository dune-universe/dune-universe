type step = {
  kick: bool;
  snare: bool;
  hihat: bool;
}

let get_length_if_equal =
  let rec get_length_if_equal' length_opt = function
    | [] -> length_opt
    | None :: rest -> get_length_if_equal' length_opt rest
    | Some str :: rest -> begin
      match length_opt with
      | None -> get_length_if_equal' (Some (String.length str)) rest
      | Some length ->
        if length = String.length str
        then get_length_if_equal' length_opt rest
        else None
    end
  in
  get_length_if_equal' None

let is_true = function
  | '1' | 'x' -> true
  | _ -> false

let is_hit_at ~index = function
  | Some pattern ->
    if index < 0 || index >= (String.length pattern)
    then false
    else is_true (String.get pattern index)
  | None -> false

let parse_patterns ~kick ~snare ~hihat =
  match get_length_if_equal [kick; snare; hihat]
  with
  | None ->
    Result.Error
      "There must be at least one drum pattern; all must be the same length"
  | Some length -> begin
    let rec compile_steps acc index =
      if index < length
      then begin
        let acc = {
          kick = is_hit_at ~index kick;
          snare = is_hit_at ~index snare;
          hihat = is_hit_at ~index hihat;
        } :: acc
        in
        compile_steps acc (index + 1)
      end
      else acc
    in
    Result.Ok (
      compile_steps [] 0
      |> List.rev
    )
  end
