open Gstreamer

let timeToString nsd =
  let msd = Int64.(to_int (div nsd (of_int 1_000_000))) in
  let h = msd / 3_600_000 in
  let m = (msd - (h * 3_600_000)) / 60_000 in
  let s = (msd - (h * 3_600_000) - (m * 60_000)) / 1000 in
  let h = if h = 0 then "" else Printf.sprintf "%d:" h in
  Printf.sprintf "%s%d:%d" h m s

let printProgression bin =
  let duration = Element.duration bin Format.Time |> timeToString in

  let rec loop () =
    let position = Element.position bin Format.Time |> timeToString in

    Printf.printf "Time : %s / %s  \r%!" position duration;

    (* Refreshing the position every seconds (in nanoseconds) *)
    let timeout = Int64.of_int 1_000_000_000 in
    let filter = [`End_of_stream; `Error] in

    match Bus.timed_pop_filtered (Bus.of_element bin) ~timeout filter with
      | exception Timeout -> loop ()
      | _ -> Printf.printf "\n"
  in
  loop ()

let () =
  Gstreamer.init ();

  let formatPipeline =
    Printf.sprintf
      "filesrc location=\"%s\" ! decodebin ! audioconvert ! audioresample ! \
       autoaudiosink"
  in
  let bin = formatPipeline Sys.argv.(1) |> Pipeline.parse_launch in

  Element.set_state bin Element.State_playing |> ignore;
  (* Wait for the state to complete. *)
  Element.get_state bin |> ignore;

  printProgression bin;

  Element.set_state bin Element.State_null |> ignore;

  Gstreamer.deinit ();
  Gc.full_major ()
