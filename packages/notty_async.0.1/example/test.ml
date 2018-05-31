open! Core
open! Async
open! Notty_async

module Pos = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  let (+) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
end

module Model : sig
  type t = private
    { dim    : Pos.t
    ; marks  : Notty.A.color Map.M(Pos).t
    ; cursor : Pos.t
    ; hover  : Pos.t option
    }
  val create      : dim:Pos.t -> cursor:Pos.t -> t
  val set_dim     : t -> Pos.t -> t
  val render      : t -> Notty.image
  val toggle_mark : t -> t
  val cursor      : t -> Pos.t
  val set_cursor  : t -> Pos.t -> t
  val set_hover   : t -> Pos.t -> t
end = struct
  type t =
    { dim : Pos.t
    ; marks : Notty.A.color Map.M(Pos).t
    ; cursor : Pos.t
    ; hover : Pos.t option
    } [@@deriving fields]

  open Notty
  open Notty.Infix

  let place img (x,y) on_ =
    I.hcat [ I.void x 1
           ; I.vcat [ I.void 1 y; img ]]
    </> on_

  let constrain_cursor t =
    let (dim_x,dim_y) = t.dim in
    let (c_x,c_y) = t.cursor in
    let cursor =
      ( Int.clamp_exn ~min:0 ~max:(dim_x - 1) c_x
      , Int.clamp_exn ~min:0 ~max:(dim_y - 1) c_y )
    in
    if Pos.(<>) cursor t.cursor then { t with cursor } else t

  let set_dim t dim =
    constrain_cursor { t with dim }

  let set_cursor t cursor =
    constrain_cursor { t with cursor }

  let set_hover t pos =
    { t with hover = Some pos }

  let create ~dim ~cursor =
    { dim; cursor; hover = None; marks = Map.empty (module Pos) }

  let rand_color () =
    let c () = Random.int 256 in
    A.rgb_888 ~r:(c ())  ~g:(c ())  ~b:(c ())

  let toggle_mark t =
    let marks =
      if Map.mem t.marks t.cursor
      then Map.remove t.marks t.cursor
      else Map.set t.marks ~key:t.cursor ~data:(rand_color ())
    in
    { t with marks }

  let base_attr t pos =
    match t.hover with
    | None -> A.empty
    | Some hover ->
      if Pos.(<>) pos hover then A.empty
      else A.bg A.blue

  let render t =
    let (x,y) = t.dim in
    let board =
      let board = I.char A.empty '.' x y in
      match t.hover with
      | None -> board
      | Some hover ->
        board
        |> place (I.string (base_attr t hover) ".") hover
    in
    Map.fold t.marks ~init:board ~f:(fun ~key:pos ~data:color board ->
        place (I.string (A.fg color) "#") pos board)
end

let run () =
  let%bind term = Term.create () in
  let events = Term.events term in
  let stop = Pipe.closed events in
  let m = ref (Model.create ~dim:(Term.size term) ~cursor:(0,0)) in
  don't_wait_for (
    Pipe.iter_without_pushback events ~f:(fun ev ->
        match ev with
        | `Mouse (`Release, pos, _) ->
          m := Model.set_cursor !m pos
        | `Mouse (`Drag, pos, _) ->
          (* This case doesn't ever seem to happen... *)
          m := Model.set_hover !m pos
        | `Key (`Arrow dir, _) ->
          let offset = match dir with
            | `Down  -> (0,1)
            | `Up    -> (0,-1)
            | `Left  -> (-1,0)
            | `Right -> (1,0)
          in
          m := Model.set_cursor !m (Pos.(+) !m.cursor offset)
        | `Key key  ->
          (match key with
           | ((`Enter | `ASCII 'x'),[]) ->
             m := Model.toggle_mark !m
           | (`ASCII 'C', [`Ctrl]) ->
             Pipe.close_read events
           | _ ->
             ())
        | `Resize _ ->
          m := Model.set_dim !m (Term.size term)
        | _ -> ()
      ));
  Clock.every' (sec 0.05) ~stop (fun () ->
      let%bind () = Term.image term (Model.render !m) in
      Term.cursor term (Some !m.cursor));
  stop

let command =
  Command.async ~summary:"Test of Notty_async"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> run ())

let () = Command.run command
