open Common
open State

let start ?(beat_number= 4) ?(beat_subdivision= 2) state =
  (* This does not work well w.r.t. to tempo changes because of the
     abosulte scale. *)
  let previous_prog = ref 0. in
  let beat = ref 0 in
  state.text_lines
  <- [|sprintf "Vimebac v. %s" Meta.version |> Bytes.of_string|] ;
  while true do
    Thread.delay 0.01 ;
    let now_ms = Unix.gettimeofday () *. 1000. |> int_of_float in
    let beat_length = 60_000 / state.bpm in
    let within_beat = now_ms mod beat_length in
    let prog = float within_beat /. float beat_length in
    if !previous_prog > prog then incr beat ;
    set_beat_progress state (if !beat mod 2 = 0 then prog else 1. -. prog) ;
    set_bar_progress state
      ( float (beat_subdivision * (!beat mod beat_number))
      +. (float beat_subdivision *. prog) ) ;
    (* printf "beat_length: %d, within_beat: %d, prog: %f beat_subdivision: %d\n%!" *)
    (*   beat_length within_beat prog beat_subdivision; *)
    let on_a_subdivision =
      List.init beat_subdivision ~f:(fun i -> i)
      |> List.find_map ~f:(fun subdiv ->
             (* printf "trying: %d against %f\n%!" subdiv prog; *)
             if
               abs_float (prog -. (float subdiv /. float beat_subdivision))
               < 0.1
             then Some subdiv
             else None )
    in
    ( match on_a_subdivision with
    | Some sub ->
        (* printf "on_a_subdivision: %d, beat: %d → %d\n%!" sub !beat *)
        (*   (beat_subdivision * (!beat mod beat_number) + sub); *)
        beat_on state ((beat_subdivision * (!beat mod beat_number)) + sub)
    | None -> beat_off state ) ;
    previous_prog := prog
  done
