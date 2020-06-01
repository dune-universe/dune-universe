open! Import
open Async
module R = Render.Make (Draw_notty)

let run_scrollable () =
  let%bind term = Notty_async.Term.create () in
  let cols, rows = Notty_async.Term.size term in
  let bounds = { Draw.r = 0; c = 0; w = 100; h = 10 } in
  let hscroll : Scroll.Scrollbar.t = Scroll.HScrollbar.create bounds in
  Scroll.Adjustment.set_range hscroll.scrollable.adj 5;
  let events = Notty_async.Term.events term in
  let stop = Pipe.closed events in
  let ctx = ref (Draw_notty.init ~rows ~cols) in
  let%bind () = Notty_async.Term.cursor term None in
  let draw ctx =
    Scroll.HScrollbar.draw ~ctx ~style:Draw.Style.default hscroll;
    let image = Draw_notty.to_image ctx in
    Notty_async.Term.image term image
  in
  let%bind () = draw !ctx in
  let handler event =
    match event with
    | `Mouse mouse -> Scroll.Scrollbar.mouse_event hscroll mouse
    | `Key key -> Scroll.Scrollbar.key_event hscroll key
    | _ -> false
  in
  let handle_event event =
    match event with
    | `Mouse _ -> handler event
    | `Key key ->
      (match key with
       | `ASCII 'q', [] | `Escape, [] ->
         Pipe.close_read events;
         false
       | _ -> handler event)
    | `Resize (_cols, _rows) ->
      (* Resizing crashes *)
      true
    | `Paste _ -> false
  in
  don't_wait_for
    (* process events in batches and draw at the end. Given rendering is slow, this
       behaves much better - especially over a ssh connection. *)
    (Pipe.iter' events ~f:(fun q ->
       let redraw =
         Core_kernel.Queue.fold q ~init:false ~f:(fun redraw event ->
           if handle_event event then true else redraw)
       in
       if redraw then draw !ctx else return ()));
  stop
;;

let () =
  Command.async
    ~summary:"Test scrollbars"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     run_scrollable)
  |> Command.run
;;
