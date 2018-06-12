open Common
open State

(**
   See the protocol in the ["README.md"] file.
*)
let start ~jack_name ~output_ports state =
  let open Misuja in
  let seq =
    let input_ports = [|"in"|] in
    Sequencer.make ~name:jack_name ~input_ports ~output_ports
  in
  let ensure_text_line index length =
    if Array.length state.text_lines <= index then (
      (* We need a space to avoid [ Failure("Error Text has zero width") ] *)
      let arr = Array.init (1 + (index * 2)) ~f:(fun _ -> Bytes.make 1 ' ') in
      Array.iteri state.text_lines ~f:(fun i a -> arr.(i) <- a) ;
      state.text_lines <- arr ) ;
    let lgth = Bytes.length (state.text_lines).(index) in
    if lgth < length then (state.text_lines).(index)
      <- Bytes.concat ""
           [(state.text_lines).(index); Bytes.make (length - lgth) ' ']
  in
  state.output_event
  <- (fun (port, stat, chan, dat1, dat2) ->
       Sequencer.output_event seq ~port ~stat ~chan ~dat1 ~dat2 ) ;
  while true do
    Thread.delay 0.002 ;
    let input = Sequencer.get_input seq in
    Array.iter input ~f:(fun (port, stat, chan, dat1, dat2) ->
        match chan with
        | 0 ->
            set_beat_progress state
              Float.(((float dat1 * 127.) + float dat2) / (128. * 127.))
        | 1 -> set_bar_progress state Float.(float dat1 + (float dat2 / 127.))
        | 2 when dat1 = 0 -> beat_on state dat2
        | 2 when dat1 = 1 -> beat_off state
        | 3 ->
            (* printf "set bpm: %x %x\n%!" dat1 dat2; *)
            state.bpm <- (dat1 * 127) + dat2
        | 4 when dat1 = 0 ->
            state.bar_structure_length <- dat2 ;
            state.bar_structure_strongs
            <- List.filter state.bar_structure_strongs ~f:(fun i -> i < dat2)
        | 4 when dat1 = 1 ->
            if List.mem dat2 ~set:state.bar_structure_strongs then ()
            else
              state.bar_structure_strongs
              <- dat2 :: state.bar_structure_strongs
        | 4 when dat1 = 2 -> state.bar_structure_strongs <- []
        | 5 when dat1 = 0 ->
            (* Printf.printf "unset line: port:%d stat:%x chan:%d dat1:%d dat2:%d\n%!" *)
            (*   port stat chan dat1 dat2; *)
            ensure_text_line dat2 0 ;
            (state.text_lines).(dat2) <- String.make 1 ' '
        | 6 | 7 ->
            (* We have 14 bits *)
            (* Printf.printf "set char: port:%d stat:%x chan:%d dat1:%d dat2:%d\n%!" *)
            (*   port stat chan dat1 dat2; *)
            let half_char = dat2 land 0b1111 in
            (* 4 bits *)
            let line_index = (dat1 land 0b0111_1000) lsr 3 in
            (* 4 bits *)
            let char_index =
              ((dat1 land 0b0111) lsl 3) + ((dat2 land 0b111_0000) lsr 4)
            in
            (* 6 bits *)
            ensure_text_line line_index (char_index + 1) ;
            let line = (state.text_lines).(line_index) in
            if chan = 6 then
              Bytes.set line char_index
                ( ((Bytes.get line char_index |> int_of_char) land 0b0000_1111)
                  + (half_char lsl 4)
                |> char_of_int )
            else
              Bytes.set line char_index
                ( ((Bytes.get line char_index |> int_of_char) land 0b1111_0000)
                  + half_char
                |> char_of_int )
        | _ ->
            Printf.printf "other: port:%d stat:%x chan:%d dat1:%d dat2:%d\n%!"
              port stat chan dat1 dat2
        (* Printf.printf "port:%d stat:%x chan:%d dat1:%d dat2:%d\n%!" *)
        (*   port stat chan dat1 dat2; *) )
  done ;
  Sequencer.close seq ;
  exit 2
