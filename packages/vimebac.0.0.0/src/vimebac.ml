open Common

module Display_option = struct
  module Geometry = struct
    let w = ref 500

    let h = ref 500

    let x = ref (None : int option)

    let y = ref None

    let x_factor = ref 0.3

    let cmdliner_term () =
      let open Cmdliner in
      let x_factor =
        let doc =
          sprintf
            "Set the relative space taken by the metronome (percentage over \
             the horizontal axis)."
        in
        let open Term in
        pure (fun v -> x_factor := float v /. 100.)
        $
        Arg.(
          value & opt int 33
          & info ["x-percent-metronome"] ~docv:"PERCENT" ~doc)
      in
      let make optname set typ default =
        let doc = sprintf "Set the window %s." optname in
        let open Term in
        pure (fun v -> set := v)
        $ Arg.(value & opt typ default & info [optname] ~docv:"INT" ~doc)
      in
      let ( ++ ) t1 t2 = Term.(pure (fun () () -> ()) $ t1 $ t2) in
      x_factor ++ make "width" w Arg.int 900
      ++ make "height" h Arg.int 300
      ++ make "x-coordinate" x Arg.(some int) None
      ++ make "y-coordinate" y Arg.(some int) None
  end
end

open State

let start_source_thread ~key_bindings ~source ~text_size =
  let state =
    { beat_progress= 0.0
    ; bar_progress= 0.0
    ; beat= None
    ; bpm= 120
    ; bar_structure_length= 8
    ; bar_structure_strongs= [0; 2; 4; 6]
    ; text_size
    ; text_lines= [||]
    ; output_event=
        (fun (port, stat, chan, dat1, dat2) ->
          printf "not outputing: port:%d stat:%x chan:%d dat1:%d dat2:%d\n%!"
            port stat chan dat1 dat2 )
    ; custom_keys= Hashtbl.create 42
    ; debug= false }
  in
  List.iter key_bindings ~f:(fun (k, m) -> Hashtbl.add state.custom_keys k m) ;
  let thread =
    match source with
    | `Jack (jack_name, output_ports) ->
        Thread.create
          (fun () -> Jack_midi_source.start ~output_ports ~jack_name state)
          ()
    | `Self (bpm, beat_number, beat_subdivision) ->
        state.bpm <- bpm ;
        state.bar_structure_length <- beat_number * beat_subdivision ;
        state.bar_structure_strongs
        <- List.init beat_number ~f:(fun i -> i * beat_subdivision) ;
        Thread.create
          (fun () -> Self_source.start ~beat_number ~beat_subdivision state)
          ()
  in
  ignore thread ; state

let with_sdl ~key_bindings ~window_title ~disable_screen_saver ~display_tempo
    ~text_size ~source () =
  let state = start_source_thread ~key_bindings ~source ~text_size in
  let open Tsdl in
  let open Tgles2 in
  (* let open Tsdl_ttf in *)
  let open Rresult in
  R.ignore_error
    ~use:(function `Msg e -> failwith (Printf.sprintf "Error %s" e))
    ( Sdl.init Sdl.Init.(timer + video + events)
    >>= fun () ->
    Sdl.create_window_and_renderer
      Sdl.Window.(windowed + opengl + resizable)
      ~w:!Display_option.Geometry.w ~h:!Display_option.Geometry.h
    >>= fun (window, renderer) ->
    Sdl.render_set_logical_size renderer !Display_option.Geometry.w
      !Display_option.Geometry.h
    >>= fun () ->
    let xy =
      match (!Display_option.Geometry.x, !Display_option.Geometry.y) with
      | Some v, Some w -> Some (v, w)
      | Some v, None -> Some (v, 0)
      | None, Some w -> Some (0, w)
      | None, None -> None
    in
    Option.iter xy ~f:(fun (x, y) -> Sdl.set_window_position window ~x ~y) ;
    Sdl.set_window_title window window_title ;
    if disable_screen_saver then Sdl.disable_screen_saver () ;
    let e = Sdl.Event.create () in
    Sdl.gl_set_attribute Sdl.Gl.doublebuffer 1
    >>= fun () ->
    Sdl.gl_create_context window
    >>= fun gl_context ->
    Sdl.gl_make_current window gl_context
    >>= fun () ->
    let wall_context =
      Wall.Renderer.create ~antialias:true ~stencil_strokes:true ()
    in
    let rec loop iteration =
      Unix.sleepf 0.010 ;
      let w, h = Sdl.get_window_size window in
      Gl.viewport 0 0 w h ;
      Gl.clear_color 1. 1. 1. 1.0 ;
      Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit)) ;
      Gl.enable Gl.blend ;
      Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha ;
      Gl.enable Gl.cull_face_enum ;
      Gl.disable Gl.depth_test ;
      let _, (mouse_x, mouse_y) = Sdl.get_mouse_state () in
      Display.render ~w ~h ~mouse_x ~mouse_y
        ~metronome_x_factor:!Display_option.Geometry.x_factor
        ~display_tempo wall_context state ;
      Sdl.gl_swap_window window ;
      match Sdl.wait_event_timeout (Some e) 50 with
      | false ->
          Sdl.delay 10l ;
          loop (iteration + 1)
      | true ->
          let key_scancode e =
            Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode)
          in
          let ctrl = Sdl.get_mod_state () land Sdl.Kmod.ctrl <> 0 in
          let event e = Sdl.Event.(enum (get e typ)) in
          let window_event e =
            Sdl.Event.(window_event_enum (get e window_event_id))
          in
          match event e with
          | `Quit -> Ok window
          | `Key_up when key_scancode e = `Q && ctrl -> Ok window
          | `Key_up ->
              (* printf "kdown: %d %d\n%!" (Sdl.get_mod_state ()) Sdl.Kmod.ctrl; *)
              ( match key_scancode e with
              | `Equals -> state.text_size <- state.text_size *. 1.1
              | `Minus -> state.text_size <- state.text_size *. 0.9
              | `J -> state.bpm <- state.bpm - 1
              | `K -> state.bpm <- state.bpm + 1
              | `H -> state.bpm <- state.bpm - 10
              | `L -> state.bpm <- state.bpm + 10
              | `P ->
                  Array.iteri state.text_lines ~f:(fun i l ->
                      printf "[%d] %S\n%!" i l ) ;
                  printf "BPM: %d\n%!" state.bpm
              (* | `Z -> *)
              (*   state.output_event (0, 0x90, 0, 45, 0); *)
              | _ ->
                  let kcode = Sdl.Event.(get e keyboard_keycode) in
                  printf "Key: %d\n%!" kcode ;
                  List.iter (Hashtbl.find_all state.custom_keys kcode) ~f:
                      (function Midi_out m -> state.output_event m ) ;
                  () ) ;
              loop (iteration + 1)
          | `Window_event -> (
            match window_event e with
            | `Exposed | `Resized ->
                let w, h = Sdl.get_window_size window in
                (* GlDraw.viewport ~x:0 ~y:0 ~w ~h; *)
                Display_option.Geometry.w := w ;
                Display_option.Geometry.h := h ;
                loop (iteration + 1)
            | _ -> loop (iteration + 1) )
          | _ -> loop (iteration + 1)
    in
    loop 0
    >>= fun window ->
    Sdl.destroy_window window ;
    (* Ttf.quit (); *)
    Sdl.gl_delete_context gl_context ;
    Sdl.quit () ;
    Ok () )

let () =
  let open Cmdliner in
  let version = Meta.version in
  let default_cmd =
    let doc = "A Visual Metronome that obeys MIDI events." in
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
    , Term.info "vimebac" ~version ~doc ~sdocs ~exits )
  in
  let font_size_term =
    let doc = "Set the initial text size (percent of height)." in
    let open Term in
    pure (fun p -> float p /. 100.)
    $ Arg.(value & opt int 8 & info ["text-size"] ~docv:"PERCENT" ~doc)
  in
  let wintitle_term =
    let doc = "Set the title of the windo." in
    let open Arg in
    value
    & opt string (sprintf "Vimebac v. %s (Ctrl-Q to quit)" version)
    & info ["win-title"] ~docv:"NAME" ~doc
  in
  let disable_screensaver =
    let doc =
      "Disable the screensaver while running (as provided by the SDL library)."
    in
    let open Arg in
    value & flag
    (* & opt string (sprintf "Vimebac v. %s (Ctrl-Q to quit)" version) *)
    & info ["disable-screen-saver"] ~docv:"NAME" ~doc
  in
  let key_bindins_term =
    let doc = "Set custom key binding." in
    let open Term in
    pure (fun all ->
        List.iter all ~f:(fun (k, (m, acs)) ->
            printf "%d → %s{%s}\n%!" k m
              (List.map acs ~f:(sprintf "%d") |> String.concat ~sep:"|") ) ;
        List.map all ~f:(fun (k, (m, acs)) ->
            match StringLabels.lowercase m with
            | "m" | "midi" ->
                ( k
                , Midi_out
                    ( match acs with
                    | [p; s; c; d1; d2] -> (p, s, c, d1, d2)
                    | _ ->
                        ksprintf failwith
                          "Error parsing CLI: Midi event should have 5 fields \
                           (%s)"
                          ( List.map acs ~f:(sprintf "%d")
                          |> String.concat ~sep:"|" ) ) )
            | _ -> ksprintf failwith "Error parsing CLI: unknown action: %S" m
        ) )
    $
    Arg.(
      value
      & opt_all (pair ~sep:':' int (pair ~sep:':' string (list int))) []
      & info ["bind-key"] ~docv:"LIST" ~doc)
  in
  let display_tempo_term =
    let doc = "Don't display the tempo." in
    Term.(pure not $ Arg.(value & flag & info ["hide-bpm"] ~doc))
  in
  let self_driven_cmd =
    let doc = "Start the metronome in “self-driven” mode." in
    let tempo_arg =
      let doc = "Set the intial tempo." in
      Arg.(value & opt int 120 & info ["T"; "bpm"] ~docv:"INT" ~doc)
    in
    let beat_number_term =
      let doc = "How many 'beats' in a bar." in
      Arg.(value & opt int 4 & info ["beats"] ~docv:"INT" ~doc)
    in
    let beat_subdivision_term =
      let doc = "How many 'subdivisions' in a beat (should be 1 or more)." in
      Arg.(value & opt int 2 & info ["subdivision"] ~docv:"INT" ~doc)
    in
    ( (let open Term in
      pure
        (fun tempo
        beats
        subs
        display_tempo
        text_size
        window_title
        disable_screen_saver
        key_bindings
        ()
        ->
          if subs <= 0 then
            ksprintf failwith "Subdivisions should be at least 1 (got %d)" subs ;
          if beats <= 0 then
            ksprintf failwith "Beats should be at least 1 (got %d)" subs ;
          with_sdl ~text_size ~window_title ~display_tempo
            ~disable_screen_saver ~key_bindings
            ~source:(`Self (tempo, beats, subs))
            () )
      $ tempo_arg $ beat_number_term $ beat_subdivision_term
      $ display_tempo_term $ font_size_term $ wintitle_term
      $ disable_screensaver $ key_bindins_term
      $ Display_option.Geometry.cmdliner_term ())
    , Term.info "self-driven" ~doc )
  in
  let jack_driven_cmd =
    let doc = "Start the metronome in “jack-driven” mode." in
    let jack_name_arg =
      let doc = "Set the name of the client from JACK's viewpoint." in
      let open Arg in
      value & opt string "vimebac" & info ["N"; "jack-name"] ~docv:"NAME" ~doc
    in
    let output_ports_term =
      let doc = "Set the names of the JACK MIDI output-ports." in
      let open Arg in
      value
      & opt (array string) [|"out"|]
      & info ["output-ports"] ~docv:"LIST" ~doc
    in
    ( (let open Term in
      pure
        (fun ()
        display_tempo
        text_size
        window_title
        disable_screen_saver
        jack_name
        output_ports
        key_bindings
        ->
          with_sdl ~text_size ~window_title ~display_tempo
            ~disable_screen_saver ~key_bindings
            ~source:(`Jack (jack_name, output_ports))
            () )
      $ Display_option.Geometry.cmdliner_term ()
      $ display_tempo_term $ font_size_term $ wintitle_term
      $ disable_screensaver $ jack_name_arg $ output_ports_term
      $ key_bindins_term)
    , Term.info "jack-driven" ~doc )
  in
  let cmds = [self_driven_cmd; jack_driven_cmd] in
  Term.(exit @@ eval_choice default_cmd cmds)
