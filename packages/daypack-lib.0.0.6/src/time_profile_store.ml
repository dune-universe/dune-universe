type t = { mutable profiles : Time_profile.data String_map.t }

let make_empty () : t = { profiles = String_map.empty }

let of_profile_list profiles =
  { profiles = profiles |> List.to_seq |> String_map.of_seq }

let matching_time_slots_of_profile =
  let cache : (string, Time_slot.t list) Hashtbl.t = Hashtbl.create 20 in
  fun ~start ~end_exc ~(profile : string) (t : t) : Time_slot.t list option ->
    match Hashtbl.find_opt cache profile with
    | None ->
      String_map.find_opt profile t.profiles
      |> Option.map (fun data ->
          let time_slots =
            Time_profile.matching_time_slots_of_data ~start ~end_exc data
            |> List.of_seq
          in
          Hashtbl.add cache profile time_slots;
          time_slots)
    | Some time_slots -> Some time_slots

let add_profile ~(profile : string) (data : Time_profile.data) (t : t) : unit =
  t.profiles <- String_map.add profile data t.profiles

module Serialize = struct
  let pack_store (t : t) : Time_profile_store_t.t =
    t.profiles
    |> String_map.to_seq
    |> Seq.map Time_profile.Serialize.pack_profile
    |> List.of_seq

  let write_to_dir ~(dir : string) (t : t) : (unit, string) result =
    try
      if Sys.is_directory dir then (
        t.profiles
        |> String_map.to_seq
        |> Seq.map (fun (name, data) ->
            (name, Time_profile.Serialize.json_string_of_data data))
        |> Seq.iter (fun (name, data) ->
            let path = Filename.concat dir (name ^ ".json") in
            let oc = open_out path in
            Fun.protect
              ~finally:(fun () -> close_out oc)
              (fun () -> output_string oc data));
        Ok () )
      else Error "File is not a directory"
    with Sys_error msg -> Error msg
end

module Deserialize = struct
  let unpack_store (t : Time_profile_store_t.t) : t =
    let profiles =
      t
      |> List.to_seq
      |> Seq.map Time_profile.Deserialize.unpack_profile
      |> String_map.of_seq
    in
    { profiles }

  let read_from_dir ~(dir : string) : (t, string) result =
    try
      let profiles =
        Sys.readdir dir
        |> Array.to_seq
        |> Seq.filter_map (fun s ->
            Filename.chop_suffix_opt ~suffix:".json" s
            |> Option.map (fun name -> (name, Filename.concat dir s)))
        |> Seq.map (fun (name, path) ->
            let ic = open_in path in
            Fun.protect
              ~finally:(fun () -> close_in ic)
              (fun () ->
                 let s = really_input_string ic (in_channel_length ic) in
                 (name, s)))
        |> Seq.map (fun (name, s) ->
            (name, Time_profile.Deserialize.data_of_json_string s))
        |> String_map.of_seq
      in
      Ok { profiles }
    with Sys_error msg -> Error msg
end

module Equal = struct
  let equal (t1 : t) (t2 : t) : bool =
    String_map.equal Time_profile.Equal.data_equal t1.profiles t2.profiles
end

module To_string = struct
  let debug_string_of_time_profile_store ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (t : t) : string =
    let open Time_profile in
    Debug_print.bprintf ~indent_level buffer "time profile store\n";
    t.profiles
    |> String_map.to_seq
    |> Seq.iter (fun (name, data) ->
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "profile : %s\n" name;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "periods :\n";
        List.iter
          (fun (start, end_exc) ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
               "start\n";
             Time_pattern.To_string.debug_string_of_time_pattern
               ~indent_level:(indent_level + 3) ~buffer start
             |> ignore;
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
               "end\n";
             Time_pattern.To_string.debug_string_of_time_pattern
               ~indent_level:(indent_level + 3) ~buffer end_exc
             |> ignore)
          data.periods);
    Buffer.contents buffer
end

module Print = struct
  let debug_print_time_profile_store ?(indent_level = 0) (t : t) =
    print_string (To_string.debug_string_of_time_profile_store ~indent_level t)
end
