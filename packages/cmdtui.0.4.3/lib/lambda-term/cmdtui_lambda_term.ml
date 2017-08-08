(******************************************************************************)
(* Copyright (c) 2016 Skylable Ltd. <info-copyright@skylable.com>             *)
(*                                                                            *)
(* Permission to use, copy, modify, and/or distribute this software for any   *)
(* purpose with or without fee is hereby granted, provided that the above     *)
(* copyright notice and this permission notice appear in all copies.          *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES   *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF           *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR    *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES     *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF    *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.             *)
(******************************************************************************)

open React
open LTerm_style
open LTerm_text
open LTerm_geom
open Lwt

let make_prompt ~prompt size =
  eval [
    S"\n";
    B_bold true;
    B_fg lcyan;
    S(Zed_utf8.make
        (size.cols)
        (CamomileLibraryDyn.Camomile.UChar.of_int 0x2500));
    E_fg;
    S"\n";

    B_fg white; S prompt; E_fg;
    B_fg lgreen; S" > "; E_fg;

    E_bold;
  ]

class read_line ~prompt ~term ~history ~commands =
  let desc, set_desc = Lwt_react.S.create None in
  object(self)
    inherit LTerm_read_line.read_line ~history () as super
    inherit [Zed_utf8.t] LTerm_read_line.term term
    val mutable executing = false

    method set_executing b =
      executing <- b

    method! message =
      Lwt_react.S.l2 (fun orig desc -> match orig, desc with
        | None, Some v -> Some v
        | Some v, _ -> Some v
        | None, None -> None
        ) super#message desc

    method! completion =
      let prefix  = Zed_rope.to_string self#input_prev in
      let split = Cmdtui.split prefix in
      match Cmdtui.completion commands split with
      | Some { desc; Cmdtui.choices = (None | Some []); _ } ->
          set_desc (match desc with
            | None ->
                None
            | Some s ->
                Some (eval [ B_bold true; S s ; E_bold ])
            );
          (*self#set_completion (String.length prefix) ["", " "]*)
          self#set_completion 0 []
      | Some { Cmdtui.choices = Some choices; _ } ->
          set_desc None;
          let partial = String.length (split.(Array.length split - 1)) in
          (* TODO: keep proper positions *)
          self#set_completion (String.length prefix - partial) (List.rev_map (fun x -> x, " ") choices)
      | _ ->
          set_desc None;
          self#set_completion 0 [];

    method! show_box =
      S.value self#mode <> LTerm_read_line.Edition || not executing

    initializer
      self#set_prompt (S.l1 (make_prompt ~prompt) self#size)
  end

module Bindings = Zed_input.Make(LTerm_key)
module StringMap = Map.Make(String)

let align3 pp_v ppf lst =
  let max_k, max_v1 = List.fold_left (fun (max1, max2) (k, (v1, _)) ->
      max max1 (String.length k),
      max max2 (String.length v1)
    ) (0,0) lst in
  let pp ppf (k, (v1, v2)) =
    Fmt.pf ppf "%-*s : %-*s -> %a" max_k k max_v1 v1 pp_v v2 in
  Fmt.(pf ppf "@[<v>%a@]" (list ~sep:Fmt.cut pp) lst)

let exit_app _ = raise Exit

let () =
  LTerm_read_line.bind [{LTerm_key.control=false;meta=false;shift=false;code=LTerm_key.F10}] [LTerm_read_line.Edit (LTerm_edit.Custom exit_app)]

let show_bindings state =
  let fold_bindings events actions accum =
    let keys = Fmt.(using LTerm_key.to_string_compact string |> list ~sep:(Fmt.unit " ") |> to_to_string) events in
    match actions with
    | [] -> StringMap.add keys ("", "no op") accum
    | lst ->
        List.fold_left (fun accum act ->
            StringMap.add keys LTerm_read_line.(name_of_action act, doc_of_action act) accum) accum lst
  in
  let rl_of_edit action = LTerm_read_line.Edit action in
  let fold_edit_bindings events actions accum =
    fold_bindings events (List.rev_map rl_of_edit actions) accum
  in
  Bindings.fold fold_edit_bindings !LTerm_edit.bindings StringMap.empty |>
  Bindings.fold fold_bindings !LTerm_read_line.bindings |>
  StringMap.bindings |>
  Fmt.(pf stdout "%a@." (align3 string));
  Lwt.return state

exception Unknown_command

let execute ~commands _ cmd =
  try
    let s = Cmdtui.split cmd in
    Logs.debug (fun m  -> m "evaluating command %a" Fmt.(Dump.array string) s);
    Cmdtui.eval commands s
  with Not_found ->
    raise Unknown_command

(* TODO: catch zed error out of bounds etc *)
let rec loop ~prompt ~commands term history state =
  Lwt.catch (fun () ->
      let rl = (new read_line ~prompt ~term ~history:(LTerm_history.contents history) ~commands) in
      rl#set_executing false;
      rl#run >>= fun command ->
      LTerm_history.add history command;
      rl#set_executing true;
      Lwt.try_bind (fun () ->
          LTerm.flush term >>= fun () ->
          let cmd = execute ~commands term (String.trim command) in
          LTerm.flush term >>= fun () ->
          cmd state
        )
        (fun result ->
           Format.pp_print_flush Format.std_formatter ();
           Format.pp_print_flush Format.err_formatter ();
           flush_all ();
           Lwt.return result)
        (function
        | Unknown_command ->
            LTerm.fprintls term (eval [B_bold true; B_fg red; S "Unknown command";E_fg; E_bold; S ": "; S command]) >>= fun () ->
            Lwt.return state
        | Exit as e -> Lwt.fail e
        | e ->
            (if Printexc.backtrace_status () then
               LTerm.fprintls term (eval [S (Printexc.get_backtrace ())])
             else
             Lwt.return_unit) >>= fun () ->
            LTerm.fprintls term (eval [B_bold true; B_fg red; S "Error";E_fg; E_bold; S ": "; S (Printexc.to_string e)]) >>= fun () ->
            Lwt.return state
        ) >>= loop ~prompt ~commands term history
    )
    (function
    | Sys.Break ->
        LTerm.fprintls term (eval [S"Interrupted"]) >>= fun () ->
        loop ~prompt ~commands term history state
    | LTerm_read_line.Interrupt as e -> Lwt.fail e
    | Exit -> Lwt.return_unit
    | e ->
        LTerm.fprintls term (eval [S (Printexc.get_backtrace ()); S"Error: "; S (Printexc.to_string e)]) >>= fun () ->
        loop ~prompt ~commands term history state)

let run_ui ~prompt ~commands state =
  LTerm_inputrc.load () >>= fun () ->
  Lwt.catch (fun () ->
      Lazy.force LTerm.stdout >>= fun term ->
      LTerm.fprintls term (eval [
          B_bold true;
          B_fg lcyan;
          S(Zed_utf8.make
              ((LTerm.size term).cols)
              (CamomileLibraryDyn.Camomile.UChar.of_int 0x2500));
          E_fg;
          E_bold;
          S"\n";
          S"\n";
          S"Type "; B_bold true; S"help"; E_bold; S" to get a list of commands, and "; B_bold true; S "bindings";E_bold;S" to get a list of keyboards bindings.\n";
          S"\n";
          S"Use "; B_bold true; S"tab"; E_bold; S" to complete the current command.\n";
          S"The completions are displayed below the prompt and are updated as you type (they are not clickable).\n"
        ]) >>= fun () ->
      loop ~prompt ~commands term (LTerm_history.create []) state
    ) (function
    | LTerm_read_line.Interrupt -> Lwt.return_unit
    | e -> fail e)

let run_lines ~prompt ~commands state lines =
  Lazy.force LTerm.stdout >>= fun term ->
  let lines = Lwt_stream.filter (function "" -> false | _ -> true) lines in
  Lwt_stream.fold_s (fun cmd state ->
      LTerm.fprints term (make_prompt ~prompt {rows=1;cols=80}) >>= fun () ->
      LTerm.fprintl term cmd >>= fun () ->
      LTerm.flush term >>= fun () ->
      execute ~commands () cmd state >>= fun state ->
      LTerm.flush term >>= fun () ->
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      flush_all ();
      Lwt.return state
    ) lines state >>= fun state ->
  LTerm.fprintl term "" >>= fun () ->
  LTerm.flush term >>= fun () ->
  Lwt.return state

let run_script ~prompt ~commands state =
  let lines = Lwt_io.read_lines Lwt_io.stdin in
  run_lines ~prompt ~commands state lines

let lwt_run f =
  let on_failure exn =
    Logs_lwt.err (fun m -> m "Uncaught exception: %a" Fmt.exn exn) >>= fun () ->
    Lwt_io.flush_all () >>= fun () ->
    exit 2
  in
  Lwt_main.run (Lwt.finalize (fun () -> Lwt.catch f on_failure) Lwt_io.flush_all)

let setup () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());

  let cancel _ = raise Sys.Break in
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Lwt_unix.on_signal Sys.sigterm cancel |> ignore;
  Lwt_unix.on_signal Sys.sigint cancel |> ignore;
  let exn_hook msg exn =
    Logs.warn (fun m -> m "%s %a" msg Fmt.exn exn)
  in
  Lwt.async_exception_hook := (exn_hook "Async exception");
  Lwt_timeout.set_exn_handler (exn_hook "Timeout exception");
  Logs.debug (fun m -> m "Initialization complete")

let run ~prompt commands state =
  setup ();
  lwt_run (fun () ->
      run_ui ~prompt ~commands state);
  exit (Logs.err_count ())

let align2 pp_v ppf lst =
  let max_length =
    List.rev_map fst lst |>
    List.rev_map String.length |>
    List.fold_left max 0
  in
  let padded_string ppf str = Fmt.pf ppf "%-*s" max_length str in
  Fmt.(pf ppf "@[<v>%a@]" (pair ~sep:(Fmt.unit "\t") padded_string pp_v |> list ~sep:Fmt.cut) lst)

let display_help descriptions state =
  Fmt.(pf stdout "%a@." (align2 string) descriptions);
  Lwt.return state
