open Gstreamer

let () =
  Gstreamer.init ();
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Please provide a file as first argument.\n%!";
    exit 1 );
  let pipeline =
    Printf.sprintf "filesrc location=\"%s\" ! decodebin ! fakesink" Sys.argv.(1)
  in
  let bin = Pipeline.parse_launch pipeline in
  (* Go in paused state. *)
  ignore (Element.set_state bin Element.State_paused);
  (* Wait for the state to complete. *)
  ignore (Element.get_state bin);
  try
    while true do
      let msg =
        Bus.pop_filtered (Bus.of_element bin) [`Error; `Tag; `Async_done]
      in
      let msg = match msg with Some msg -> msg | None -> raise Exit in
      Printf.printf "Message from %s\n%!" msg.source;
      match msg.payload with
        | `Error e -> failwith ("Error message: " ^ e)
        | `Tag tags ->
            List.iter
              (fun (l, v) ->
                Printf.printf "- %s : %s\n%!" l (String.concat ", " v))
              tags
        | _ -> ()
    done
  with Exit -> ()
